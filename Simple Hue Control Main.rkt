#lang racket/gui

(require net/http-client
         net/uri-codec
         json
         "shc-classes.rkt"
         "shc-settings.rkt"
         "shc-show_control.rkt")

(compile-allow-set!-undefined #t)

; Creates a folder in ~/Library/Application Support/ if one does not exist.
; Create Bridge Address and User Name files if they do not exist. Otherwise,
; open the files.

(supportDirectoryExists?)

(bridgeSettingsFileExists?)

; Bridge Communication Variables. Communication will not work until 
; these are set by the user.

; Need to set up error handling if the user tries to use the application
; before setting these.

(define bridgeAddress (hash-ref (file->value bridgeSettingsFile) 'bridge-address))
(define userDeviceName (hash-ref (file->value bridgeSettingsFile) 'user-device))
(define hueUserName (hash-ref (file->value bridgeSettingsFile) 'hue-user-name))
(define deviceType (hash-ref (file->value bridgeSettingsFile) 'device-type))
(define appName (hash-ref (file->value bridgeSettingsFile) 'app-name))

; Create a main Cue List. Temporary. Eventually there will be an option for
;; multiple cue lists.

(define mainList (new cueList% [label "Main List"]))

;; Create a patch object.

(define mainPatch (new patch% [label "Main Patch"]))

;; Create lights.

(for ([i (in-range 1 17)])
  (new light%
       [label (string-append "Hue-" (number->string i))]
       [parent mainPatch]
       [bulb i]
       [group 0]))

; Initialize variables for lights to send commands to and the lighting
; state to send.
(define lightsToCue '(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
;(define lightingState '(#f 1 0 0))
(define lightingState
  (make-hash
   (hash->list
    (hash
     'on #f
     'onChange #f
     'bri 1
     'briChange #f
     'hue 0
     'hueChange #f
     'sat 0
     'satChange #f))))

; We need to update the Status Window with the lights just used.
; This is for the "Lighting Status" Window. Data does not come from
; the bridge. Rather from the last command sent.

(define lastLightsMessage "Lights: ")
(define lastOnMessage "On?: ")
(define lastBriMessage "Intensity: ")
(define lastHueMessage "Hue: ")
(define lastSatMessage "Saturation: ")
(define lastTransitiontimeMessage "Cue Time: ")

(define updateLastLights
  (lambda (lights)
    (let ([numberOfLights (length lights)])
      (set! lastLightsMessage "Lights: ")
      (for ([i (in-range numberOfLights)])
        (set! lastLightsMessage (string-append lastLightsMessage (string-append (number->string (list-ref lights i)) ", "))))
      (set! lastLightsMessage (string-trim lastLightsMessage ", ")))
    (send lastLightsDisplay set-label lastLightsMessage)))

(define updateOn
  (lambda (state)
    (cond
      ((equal? (hash-ref state 'on) #t) (set! lastOnMessage "On?: TRUE"))
      ((equal? (hash-ref state 'on) #f) (set! lastOnMessage "On?: FALSE"))
      (else (set! lastOnMessage "On? ")))
    (send lastOnDisplay set-label lastOnMessage)))

(define updateBri
  (lambda (state)
    (set! lastBriMessage (string-append "Intensity: " (number->string (hash-ref state 'bri))))
    (send lastBriDisplay set-label lastBriMessage)))

(define updateHue
  (lambda (state)
    (set! lastHueMessage (string-append "Hue: " (number->string (hash-ref state 'hue))))
    (send lastHueDisplay set-label lastHueMessage)))

(define updateSat
  (lambda (state)
    (set! lastSatMessage (string-append "Saturation: " (number->string (hash-ref state 'sat))))
    (send lastSatDisplay set-label lastSatMessage)))

(define updateLastTransitiontime
  (lambda (time)
    (cond
      ((= cueTime 0) (set! lastTransitiontimeMessage "Cue Time: 0 seconds"))
      (else (set! lastTransitiontimeMessage (string-append (string-append "Cue Time: " (number->string (/ time 10))) " seconds"))))
    (send lastTransitiontimeDisplay set-label lastTransitiontimeMessage)))

(define updateLastStatus
  (lambda (lights state time)
    (updateLastLights lights)
    (updateOn state)
    (updateBri state)
    (updateHue state)
    (updateSat state)
    (updateLastTransitiontime time)))

; Now it is time to create the main interaction window.

(define hueWindow (new frame% [label "Simple Hue Control"]))

; Next we create the panel to select the lights to control.

(define lightsSelect (new vertical-panel% [parent hueWindow]
                          [style '(border)]))

(define lightsSelectTitle (new horizontal-panel% [parent lightsSelect]
                               [alignment '(center center)]))
(new message% [parent lightsSelectTitle]
     [label "Select Lights to Cue"])

(define lightsSelectPanelTop (new horizontal-panel% [parent lightsSelect]
                                  [alignment '(center center)]
                                  [spacing 10]))
(define lightsSelectPanelBottom (new horizontal-panel% [parent lightsSelect]
                                     [alignment '(center center)]
                                     [border 4]
                                     [spacing 3]))

(define lightsSelectPanelButtons (new horizontal-panel% [parent lightsSelect]
                                      [alignment '(center center)]
                                      [border 4]
                                      [spacing 3]))

(define lightsSelectPanelAccessButtons (new horizontal-panel% [parent lightsSelectPanelButtons]
                                            [alignment '(left center)]
                                            [border 4]
                                            [spacing 3]))

(define lightsSelectPanelSelectButton (new horizontal-panel% [parent lightsSelectPanelButtons]
                                           [alignment '(right center)]
                                           [border 4]
                                           [spacing 3]))

; Create some check boxes to select lights.
(for ([i (in-range 1 9)])
  (new check-box% [parent lightsSelectPanelTop]
       [label (string-append "LX " (~a i))]
       [value #f]))
(for ([i (in-range 9 17)])
  (new check-box% [parent lightsSelectPanelBottom]
       [label (string-append "LX " (~a i))]
       [value #f]))

(define lightsSelectButton (new button% [parent lightsSelectPanelSelectButton]
                                [label "Lights!"]
                                [callback (lambda (button event)
                                            (set! lightsToCue (getLights
                                                               (send lightsSelectPanelTop get-children)
                                                               (send lightsSelectPanelBottom get-children))))]))

(define lightsClearButton (new button% [parent lightsSelectPanelAccessButtons]
                               [label "Clear"]
                               [callback (lambda (button event)
                                           (for ([i (in-range 8)])
                                             (send (list-ref (send lightsSelectPanelTop get-children) i) set-value #f))
                                           (for ([i (in-range 8)])
                                             (send (list-ref (send lightsSelectPanelBottom get-children) i) set-value #f)))]))

(define lightsSelectAllButton (new button% [parent lightsSelectPanelAccessButtons]
                                   [label "Select All"]
                                   [callback (lambda (button event)
                                               (for ([i (in-range 8)])
                                                 (send (list-ref (send lightsSelectPanelTop get-children) i) set-value #t))
                                               (for ([i (in-range 8)])
                                                 (send (list-ref (send lightsSelectPanelBottom get-children) i) set-value #t)))]))

; Next is the panel for setting the attributes.
(define lightsAttributes (new vertical-panel% [parent hueWindow]
                              [style '(border)]))
(define lightsOn (new horizontal-panel% [parent lightsAttributes]))
(define lightsChange (new radio-box% [parent lightsOn]
                          [label "On or Off?"]
                          [choices '("On" "Off")]
                          [style '(horizontal)]))

(define lightsBri (new horizontal-panel% [parent lightsAttributes]))
(define lightsIntensity (new slider% [parent lightsBri]
                             [label "Intensity?"]
                             [min-value 1]
                             [max-value 254]
                             [init-value 1]
                             [style '(horizontal)]
                             [min-width 10]
                             [stretchable-width 10]))

(define lightsCol (new horizontal-panel% [parent lightsAttributes]))
(define lightsColor (new slider% [parent lightsCol]
                         [label "Hue?"]
                         [min-value 0]
                         [max-value 65535]
                         [init-value 0]
                         [style '(horizontal)]
                         [min-width 10]
                         [stretchable-width 10]))

(define lightsSat (new horizontal-panel% [parent lightsAttributes]))
(define lightsSaturation (new slider% [parent lightsSat]
                              [label "Saturation?"]
                              [min-value 0]
                              [max-value 254]
                              [init-value 0]
                              [style '(horizontal)]
                              [min-width 10]
                              [stretchable-width 10]))

(define lightsAttributesButtonPanel (new horizontal-panel% [parent lightsAttributes]
                                         [alignment '(right center)]))

(define lightsAttributesButton (new button% [parent lightsAttributesButtonPanel]
                                    [label "Lighting State!"]
                                    [callback (lambda (button event)
                                                (let ([light-objects (lightList lightsToCue)])
                                                  (for ([i (in-range (length light-objects))])
                                                    (send (list-ref
                                                           (send mainPatch get-children)
                                                           (- (list-ref light-objects i) 1))
                                                          set-state (make-hash
                                                                     (append
                                                                      (on-pair lightsChange)
                                                                      (bri-pair lightsIntensity)
                                                                      (hue-pair lightsColor)
                                                                      (sat-pair lightsSaturation)))))))]))

; Now we set the cue time.

(define cueTime 1)

(define cueTimePanel (new horizontal-panel% [parent hueWindow]
                          [style '(border)]))

(define cueTimeField (new text-field% [parent cueTimePanel]
                          [label "Cue Time in Seconds"]))

(define setCueTimeButton (new button% [parent cueTimePanel]
                              [label "Set Time!"]
                              [callback (lambda (button event)
                                          (cond
                                            ((not (number? (string->number (send cueTimeField get-value))))
                                             (set! cueTime 0))
                                            (else
                                             (set! cueTime
                                                   (inexact->exact (* (string->number (send cueTimeField get-value)) 10))))))]))

; Now we need to send the cue to the bridge and save the cue.
; The Save Cue button saves the current lighting state. NOT the one about
; to be sent.

(define cueGoAndSavePanel (new horizontal-panel% [parent hueWindow]
                               [style '(border)]
                               [alignment '(center center)]))

(define cueSavePanel (new horizontal-panel% [parent cueGoAndSavePanel]
                          [alignment '(left center)]))

(define cueSaveButton (new button% [parent cueSavePanel]
                           [label "Save"]
                           [min-height 50]
                           [callback (lambda (button event)
                                       (send saveCueDialog show #t))]))
; Create A Dialog for Saving Cues.

(define saveCueDialog (new dialog% [parent hueWindow]
                           [label "Save Cue"]))
(define saveCueNamePanel (new horizontal-panel% [parent saveCueDialog]
                              [alignment '(left center)]
                              [min-width 200]))
(define saveCueNameField (new text-field% [parent saveCueNamePanel]
                              [label "Cue Name:"]))
(define saveCueTimeField (new text-field% [parent saveCueNamePanel]
                              [label "Cue Time:"]))

(define saveCueButtonPanel (new horizontal-panel% [parent saveCueDialog]
                                [alignment '(right center)]
                                [min-width 200]))

(define saveCueCancelButton (new button% [parent saveCueButtonPanel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             (send saveCueNameField set-value "")
                                             (send saveCueDialog show #f))]))

(define saveCueOKButton (new button% [parent saveCueButtonPanel]
                             [label "Save"]
                             [callback (lambda(button event)
                                         (let [(newCueName (send saveCueNameField get-value))]
                                           (new cue% [label newCueName]
                                                [parent mainList])
                                           (let [(newCuePosition (- (length (send mainList get-children)) 1))]
                                             (send (list-ref (send mainList get-children) newCuePosition) set-json (retrieveBridgeStatus bridgeAddress hueUserName))
                                             (send (list-ref (send mainList get-children) newCuePosition) set-time (send saveCueTimeField get-value)))
                                           (send cueChoice append newCueName)
                                           (send saveCueNameField set-value "")
                                           (send saveCueDialog show #f)))]))

; Create Go Button

(define cueGoPanel (new horizontal-panel% [parent cueGoAndSavePanel]
                        [alignment '(right center)]))

(define cueGoButton (new button% [parent cueGoPanel]
                         [label "GO!"]
                         [min-height 50]
                         [callback (lambda (button event)
                                     (goLights (lightList lightsToCue) mainPatch cueTime bridgeAddress hueUserName)
                                     (updateLastStatus (lightList lightsToCue) lightingState cueTime)
                                     (updateAllLights
                                      1 16
                                      lights1To8
                                      lights9To16
                                      bridgeAddress
                                      hueUserName))]))

; Now we need a Status Window.

(define statusWindow (new frame% [label "Lighting Status"]
                          [min-width 550]))

; Display the last lighting change.

(define lastLightsGroup (new group-box-panel% [parent statusWindow]
                             [label "Last Lights & State"]
                             [alignment '(left top)]))

(define lastLightsDisplay (new message% [parent lastLightsGroup]
                               [label lastLightsMessage]
                               [auto-resize #t]))

(define lastOnDisplay (new message% [parent lastLightsGroup]
                           [label lastOnMessage]
                           [auto-resize #t]))

(define lastBriDisplay (new message% [parent lastLightsGroup]
                            [label lastBriMessage]
                            [auto-resize #t]))

(define lastHueDisplay (new message% [parent lastLightsGroup]
                            [label lastHueMessage]
                            [auto-resize #t]))

(define lastSatDisplay (new message% [parent lastLightsGroup]
                            [label lastSatMessage]
                            [auto-resize #t]))

(define lastTransitiontimeDisplay (new message% [parent lastLightsGroup]
                                       [label lastTransitiontimeMessage]
                                       [auto-resize #t]))

; Finally we need a window to show the status of all the lights.
; This Window gets its data from the bridge.

(define allLights (new frame% [label "All Lights"]
                       [min-width 1000]))

; The first eight lights

(define lights1To8 (new horizontal-panel% [parent allLights]
                        [alignment '(left top)]))

(define light1 (new group-box-panel% [parent lights1To8]
                    [label "LX 1"]
                    [alignment '(left top)]))
(define light1On (new message% [parent light1]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light1Bri (new message% [parent light1]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light1Hue (new message% [parent light1]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light1Sat (new message% [parent light1]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light2 (new group-box-panel% [parent lights1To8]
                    [label "LX 2"]
                    [alignment '(left top)]))
(define light2On (new message% [parent light2]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light2Bri (new message% [parent light2]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light2Hue (new message% [parent light2]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light2Sat (new message% [parent light2]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light3 (new group-box-panel% [parent lights1To8]
                    [label "LX 3"]
                    [alignment '(left top)]))
(define light3On (new message% [parent light3]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light3Bri (new message% [parent light3]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light3Hue (new message% [parent light3]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light3Sat (new message% [parent light3]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light4 (new group-box-panel% [parent lights1To8]
                    [label "LX 4"]
                    [alignment '(left top)]))
(define light4On (new message% [parent light4]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light4Bri (new message% [parent light4]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light4Hue (new message% [parent light4]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light4Sat (new message% [parent light4]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light5 (new group-box-panel% [parent lights1To8]
                    [label "LX 5"]
                    [alignment '(left top)]))
(define light5On (new message% [parent light5]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light5Bri (new message% [parent light5]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light5Hue (new message% [parent light5]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light5Sat (new message% [parent light5]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light6 (new group-box-panel% [parent lights1To8]
                    [label "LX 6"]
                    [alignment '(left top)]))
(define light6On (new message% [parent light6]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light6Bri (new message% [parent light6]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light6Hue (new message% [parent light6]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light6Sat (new message% [parent light6]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light7 (new group-box-panel% [parent lights1To8]
                    [label "LX 7"]
                    [alignment '(left top)]))
(define light7On (new message% [parent light7]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light7Bri (new message% [parent light7]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light7Hue (new message% [parent light7]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light7Sat (new message% [parent light7]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light8 (new group-box-panel% [parent lights1To8]
                    [label "LX 8"]
                    [alignment '(left top)]))
(define light8On (new message% [parent light8]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light8Bri (new message% [parent light8]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light8Hue (new message% [parent light8]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light8Sat (new message% [parent light8]
                       [label initialSatMessage]
                       [auto-resize #t]))

; The next eight lights

(define lights9To16 (new horizontal-panel% [parent allLights]
                         [alignment '(left top)]))

(define light9 (new group-box-panel% [parent lights9To16]
                    [label "LX 9"]
                    [alignment '(left top)]))
(define light9On (new message% [parent light9]
                      [label initialOnMessage]
                      [auto-resize #t]))
(define light9Bri (new message% [parent light9]
                       [label initialBriMessage]
                       [auto-resize #t]))
(define light9Hue (new message% [parent light9]
                       [label initialHueMessage]
                       [auto-resize #t]))
(define light9Sat (new message% [parent light9]
                       [label initialSatMessage]
                       [auto-resize #t]))

(define light10 (new group-box-panel% [parent lights9To16]
                     [label "LX 10"]
                     [alignment '(left top)]))
(define light10On (new message% [parent light10]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light10Bri (new message% [parent light10]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light10Hue (new message% [parent light10]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light10Sat (new message% [parent light10]
                        [label initialSatMessage]
                        [auto-resize #t]))

(define light11 (new group-box-panel% [parent lights9To16]
                     [label "LX 11"]
                     [alignment '(left top)]))
(define light11On (new message% [parent light11]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light11Bri (new message% [parent light11]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light11Hue (new message% [parent light11]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light11Sat (new message% [parent light11]
                        [label initialSatMessage]
                        [auto-resize #t]))

(define light12 (new group-box-panel% [parent lights9To16]
                     [label "LX 12"]
                     [alignment '(left top)]))
(define light12On (new message% [parent light12]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light12Bri (new message% [parent light12]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light12Hue (new message% [parent light12]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light12Sat (new message% [parent light12]
                        [label initialSatMessage]
                        [auto-resize #t]))

(define light13 (new group-box-panel% [parent lights9To16]
                     [label "LX 13"]
                     [alignment '(left top)]))
(define light13On (new message% [parent light13]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light13Bri (new message% [parent light13]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light13Hue (new message% [parent light13]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light13Sat (new message% [parent light13]
                        [label initialSatMessage]
                        [auto-resize #t]))

(define light14 (new group-box-panel% [parent lights9To16]
                     [label "LX 14"]
                     [alignment '(left top)]))
(define light14On (new message% [parent light14]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light14Bri (new message% [parent light14]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light14Hue (new message% [parent light14]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light14Sat (new message% [parent light14]
                        [label initialSatMessage]
                        [auto-resize #t]))

(define light15 (new group-box-panel% [parent lights9To16]
                     [label "LX 15"]
                     [alignment '(left top)]))
(define light15On (new message% [parent light15]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light15Bri (new message% [parent light15]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light15Hue (new message% [parent light15]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light15Sat (new message% [parent light15]
                        [label initialSatMessage]
                        [auto-resize #t]))

(define light16 (new group-box-panel% [parent lights9To16]
                     [label "LX 16"]
                     [alignment '(left top)]))
(define light16On (new message% [parent light16]
                       [label initialOnMessage]
                       [auto-resize #t]))
(define light16Bri (new message% [parent light16]
                        [label initialBriMessage]
                        [auto-resize #t]))
(define light16Hue (new message% [parent light16]
                        [label initialHueMessage]
                        [auto-resize #t]))
(define light16Sat (new message% [parent light16]
                        [label initialSatMessage]
                        [auto-resize #t]))

; Create a Window for the Cue List.

(define cueListWindow (new frame% [label "Main Cue List"]))

(define cueListPanel (new vertical-panel% [parent cueListWindow]
                          [alignment '(left center)]
                          [min-width 250]))

(define cueChoice (new choice% [parent cueListPanel]
                       [label "Cues:"]
                       [min-width 230]
                       [choices '()]))

(define restoreAndDeletePanel (new horizontal-panel% [parent cueListPanel]
                                   [alignment '(center center)]))

(define deletePanel (new horizontal-panel% [parent restoreAndDeletePanel]
                         [alignment '(left center)]))

(define deleteButton (new button% [parent deletePanel]
                          [label "Delete"]
                          [callback (lambda (button event)
                                      (deleteCue 
                                       mainList 
                                       (send cueChoice get-selection))
                                      (send cueChoice delete (send cueChoice get-selection)))]))

(define restorePanel (new horizontal-panel% [parent restoreAndDeletePanel]
                          [alignment '(right center)]))

(define restoreButton (new button% [parent restorePanel]
                           [label "Restore"]
                           [callback (lambda (button event)
                                       (restoreCue 
                                        mainList 
                                        (send cueChoice get-selection) 
                                        17
                                        bridgeAddress
                                        hueUserName)
                                       (updateAllLights
                                        1
                                        16
                                        lights1To8
                                        lights9To16
                                        bridgeAddress
                                        hueUserName))]))

; Menu Bars

; For Hue Window

(define hueWindowMenuBar (new menu-bar% [parent hueWindow]))

; Bridge Menu
(define hueWindowMenuBridge (new menu% [parent hueWindowMenuBar]
                                 [label "Bridge"]))
(define hueWindowMenuBridgeBridgeAddress (new menu-item% [parent hueWindowMenuBridge]
                                              [label "Set Bridge Address…"]
                                              [callback (lambda (menu event)
                                                          (send bridgeAddressDialog show #t))]))
(define hueWindowMenuBridgeUserName (new menu-item% [parent hueWindowMenuBridge]
                                         [label "Set User Name…"]
                                         [callback (lambda (menu event)
                                                     (send userNameDialog show #t))]))

(define hueWindowMenuBridgeUpdateFirmware (new menu-item% [parent hueWindowMenuBridge]
                                               [label "Update Bridge Firmware"]
                                               [callback (lambda (menu event)
                                                           (send updateFirmwareDialog show #t))]))

; Set Bridge Address Dialog
(define bridgeAddressDialog (new dialog% [label "Enter Bridge Address"]
                                 [min-width 300]
                                 [min-height 100]))
(define bridgeAddressPanel (new horizontal-panel% [parent bridgeAddressDialog]
                                [alignment '(left center)]
                                [min-width 300]))
(define bridgeAddressField (new text-field% [parent bridgeAddressPanel]
                                [label "Bridge Address:"]
                                [init-value bridgeAddress]
                                [horiz-margin 20]))
(define setBridgeAddressPanel (new horizontal-panel% [parent bridgeAddressDialog]
                                   [alignment '(center center)]
                                   [min-width 300]))
(define cancelBridgeAddress (new button% [parent setBridgeAddressPanel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             ;(send bridgeAddressField set-value "0.0.0.0")
                                             (send bridgeAddressDialog show #f))]))
(define saveBridgeAddress (new button% [parent setBridgeAddressPanel]
                               [label "Save"]
                               [callback (lambda (button event)
                                           (set! bridgeAddress (send bridgeAddressField get-value))
                                           (let ([bridgeSettings (make-hash (hash->list (file->value bridgeSettingsFile)))])
                                             (hash-set! bridgeSettings 'bridgeAddress (send bridgeAddressField get-value))
                                             (with-output-to-file bridgeSettingsFile
                                               (lambda () (write bridgeSettings))
                                               #:mode 'text
                                               #:exists 'replace)
                                             (send bridgeAddressDialog show #f)))]))

; Set Bridge User Name Dialog

(define bridgeError "")

(define setUserName!
  (lambda (device)
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   bridgeAddress "/api"
                   #:method 'POST
                   #:data
                   (jsexpr->string
                    (hash 'devicetype device))
                   #:headers
                   '("Content-Type: application/json")
                   #:content-decode '(json))])
      (let ([bridgeResponse (read-json jsonResponse)])
        (cond
          ((equal? (hash-keys (car bridgeResponse)) '(error))
           (set! bridgeError (string-append "Error: " (hash-ref (hash-ref (car bridgeResponse) 'error) 'description))))
          ((equal? (hash-keys (car bridgeResponse)) '(success))
           (set! hueUserName (hash-ref (hash-ref (car bridgeResponse) 'success) 'username))
           (set! bridgeError "")))))))


(define userNameDialog (new dialog% [label "Enter Hue Bridge User Name"]
                            [min-width 550]
                            [min-height 100]))
(define userNameMessagePanel (new vertical-panel% [parent userNameDialog]
                                  [alignment '(center center)]
                                  [min-width 300]))
(define userNameMessage (new message% [parent userNameMessagePanel]
                             [label "Enter Device Name (ie: My Macbook). Press Link Button on Bridge. Click \"Set\"."]
                             [vert-margin 10]
                             [horiz-margin 20]
                             [auto-resize #t]))
(define userNamePanel (new vertical-panel% [parent userNameDialog]
                           [alignment '(left center)]
                           [min-width 200]
                           [stretchable-width 200]))
(define userDeviceNameField (new text-field% [parent userNamePanel]
                                 [label "Device Name:"]
                                 [init-value userDeviceName]
                                 [horiz-margin 50]
                                 [min-width 200]
                                 [stretchable-width 200]))
(define setUserNamePanel (new horizontal-panel% [parent userNameDialog]
                              [alignment '(center center)]
                              [min-width 320]))
(define cancelUserName (new button% [parent setUserNamePanel]
                            [label "Cancel"]
                            [callback (lambda (button event)
                                        ;(send userDeviceNameField set-value "")
                                        (send userNameDialog show #f))]))
(define saveUserName (new button% [parent setUserNamePanel]
                          [label "Set"]
                          [callback (lambda (button event)
                                      (let ([bridgeSettings
                                             (make-hash
                                              (hash->list
                                               (file->value bridgeSettingsFile)))])
                                        (set! userDeviceName 
                                              (send userDeviceNameField get-value))
                                        (hash-set! bridgeSettings
                                                   'userDevice
                                                   userDeviceName)
                                        (set! deviceType (string-append appName (string-append "#" userDeviceName)))
                                        (hash-set! bridgeSettings
                                                   'deviceType
                                                   deviceType)
                                        (setUserName! deviceType)
                                        (cond
                                          ((equal? bridgeError "")
                                           (hash-set! bridgeSettings
                                                      'hueUserName
                                                      hueUserName)
                                           (with-output-to-file bridgeSettingsFile
                                             (lambda () (write bridgeSettings))
                                             #:mode 'text
                                             #:exists 'replace)
                                           (send userNameDialog show #f))
                                          (else
                                           (send userNameMessage set-label
                                                 (string-append 
                                                  bridgeError 
                                                  ". Enter Device Name (ie: My Macbook). \n Press Link Button on Bridge. Click \"Set\"."))))))]))

; Bridge Update Dialog

(define portalError "Error: Portal Connection Unavailable. Check the bridge's internet connection. Is the third light a steady blue?")
(define updatingError "Error: Bridge Is Currently Updating. Please Wait for Blue Lights to Return to Normal")

(define updateBridge
  (lambda ()
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   bridgeAddress (string-append 
                                  (string-append "/api/" hueUserName) 
                                  "/config/") 
                   #:method 'GET
                   #:headers
                   '("Content-Type: application/json")
                   #:content-decode '(json))])
      (let ([bridgeResponse (read-json jsonResponse)])
        (cond
          ((equal? (hash-ref (hash-ref bridgeResponse 'portalstate) 'signedon) #t)
           (cond
             ((equal? (hash-ref (hash-ref (hash-ref bridgeResponse 'swupdate) 'devicetypes) 'bridge) #t)
              (cond
                ; Possible Responses:
                ; 0 = No update available
                ; 1 = Downloading updates
                ; 2 = Updates are ready to be installed
                ; 3 = Installing updates
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 0)
                 ; Set 'checkforupdate to #t.
                 (let-values ([(httpStatus2 httpHeader2 jsonResponse2)
                               (http-sendrecv
                                bridgeAddress (string-append 
                                               (string-append "/api/" hueUserName) 
                                               "/config/") 
                                #:method 'PUT
                                #:data
                                (jsexpr->string
                                 (hash 'swupdate
                                       (hash 'checkforupdate #t)))
                                #:headers
                                '("Content-Type: application/json")
                                #:content-decode '(json))])
                   (let ([bridgeResponse2 (read-json jsonResponse2)])
                     (cond 
                       ((equal? (hash-keys (car bridgeResponse2)) '(error))
                        (set! bridgeError (string-append "Error: " 
                                                         (hash-ref 
                                                          (hash-ref (car bridgeResponse2) 'error) 
                                                          'description))))
                       ((equal? (hash-keys (car bridgeResponse2)) '(success))
                        (set! bridgeError "Error: No Update Available. Will Check."))))))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 1)
                 (set! bridgeError "Error: Update Still Downloading. Please Wait."))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 2)
                 ; Need to initiate Update.
                 (let-values ([(httpStatus2 httpHeader2 jsonResponse2)
                               (http-sendrecv
                                bridgeAddress (string-append 
                                               (string-append "/api/" hueUserName) 
                                               "/config/") 
                                #:method 'PUT
                                #:data
                                (jsexpr->string
                                 (hash 'swupdate
                                       (hash 'updatestate 3)))
                                #:headers
                                '("Content-Type: application/json")
                                #:content-decode '(json))])
                   (let ([bridgeResponse2 (read-json jsonResponse2)])
                     (cond 
                       ((equal? (hash-keys (car bridgeResponse2)) '(error))
                        (set! bridgeError (string-append "Error: " 
                                                         (hash-ref 
                                                          (hash-ref (car bridgeResponse2) 'error) 
                                                          'description))))
                       ((equal? (hash-keys (car bridgeResponse2)) '(success))
                        (set! bridgeError "Done."))))))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 3)
                 (set! bridgeError updatingError))))
             ((equal? (hash-ref (hash-ref (hash-ref bridgeResponse 'swupdate) 'devicetypes) 'bridge) #f)
              (set! bridgeError "No Update Available."))))
          (else (set! bridgeError portalError)))))))

(define updateFirmwareDialog (new dialog% [label "Update Bridge Firmware"]
                                  [min-width 350]
                                  [min-height 100]))

(define updateFirmwareMessagePanel (new vertical-panel% [parent updateFirmwareDialog]
                                        [alignment '(center center)]
                                        [min-width 200]))
(define updateFirmwareMessage (new message% [parent updateFirmwareMessagePanel]
                                   [label "Update Firmware? Bridge Must Be Connected to the Internet."]
                                   [horiz-margin 20]))

(define updateFirmwarePanel (new horizontal-panel% [parent updateFirmwareDialog]
                                 [alignment '(center center)]
                                 [min-width 290]))
(define closeUpdateFirmware (new button% [parent updateFirmwarePanel]
                                 [label "Close"]
                                 [callback (lambda (button event)
                                             (send updateFirmwareDialog show #f))]))
(define updateFirmware (new button% [parent updateFirmwarePanel]
                            [label "Update"]
                            [callback (lambda (button event)
                                        (updateBridge)
                                        (send updateFirmwareMessage set-label bridgeError))]))

; Windows Menu
(define hueWindowMenuWindows (new menu% [parent hueWindowMenuBar]
                                  [label "Windows"]))
(define hueWindowMenuWindowsNum1 (new menu-item% [parent hueWindowMenuWindows]
                                      [label (send hueWindow get-label)]
                                      [callback (lambda (menu event)
                                                  (send hueWindow iconize #f))]
                                      [shortcut #\1]
                                      [shortcut-prefix '(cmd)]))
(define hueWindowMenuWindowsNum2 (new menu-item% [parent hueWindowMenuWindows]
                                      [label (send statusWindow get-label)]
                                      [callback (lambda (menu event)
                                                  (send statusWindow iconize #f))]
                                      [shortcut #\2]
                                      [shortcut-prefix '(cmd)]))
(define hueWindowMenuWindowsNum3 (new menu-item% [parent hueWindowMenuWindows]
                                      [label (send allLights get-label)]
                                      [callback (lambda (menu event)
                                                  (send allLights iconize #f))]
                                      [shortcut #\3]
                                      [shortcut-prefix '(cmd)]))
(define hueWindowMenuWindowsNum4 (new menu-item% [parent hueWindowMenuWindows]
                                      [label (send cueListWindow get-label)]
                                      [callback (lambda (menu event)
                                                  (send cueListWindow iconize #f))]
                                      [shortcut #\4]
                                      [shortcut-prefix '(cmd)]))

; Show the Windows

(send statusWindow show #t)
(send allLights show #t)
(send cueListWindow show #t)
(send hueWindow show #t)
