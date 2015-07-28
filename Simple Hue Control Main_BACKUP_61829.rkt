#lang racket/gui

(require net/http-client
         net/uri-codec
         json)

(compile-allow-set!-undefined #t)

; Hack for Workshop—Set Bridge Address to Hue Bridge static address.
; Eventually add a menu item to set this value.
; Also needed is a menu item to set the Hue Bridge user name.

(define bridgeAddress "192.168.1.95")
(define hueUserName "brucelighting")

; Cue List and Cue Classes

(define cueList%
  (class object%
    (super-new)
    (init-field [label ""])
    (init-field [children '()])
    (define/public (get-label) label)
    (define/public (get-children) children)
    (define/public (set-children listOfChildren)
      (set-field! children this listOfChildren))))

(define cue%
  (class object%
    (super-new)
    (define stateJson     (hash
                           'on #t
                           'bri 0
                           'hue 0
                           'sat 0
                           'xy (list 0 0)
                           'ct 0
                           'alert "none"
                           'effect "none"
                           'colormode "hs"
                           'reachable #t))
    (init-field [label ""])
    (init-field [jsonResponse (hash 'light stateJson
                                    'type "Extended color light"
                                    'name "Generic Name"
                                    'modelid "LCT001"
                                    'swversion "")])
    (init-field [parent '(object:cueList%)])
    (define/public (get-label) label)
    (define/public (get-parent) parent)
    (define/public (get-json) jsonResponse)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-json newJson)
      (set-field! jsonResponse this newJson))
    (define/public (set-parent parentCueList)
      (set-field! children parentCueList 
                  (append (get-field children parentCueList) (list this)))
      (set-field! parent this parentCueList))))

; Hack for Workshop: Create a main Cue List.

(define mainList (new cueList% [label "Main List"]))

; Creating the procedures we will need.

; First is getting the lights we need to cue.
(define lightsToCue '(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define getLightsRow 
  (lambda (panelContents)
    (cond
      ((null? panelContents) (quote ()))
      (else (cons (send (car panelContents) get-value) (getLightsRow (cdr panelContents)))))))

(define getLights 
  (lambda (firstRow secondRow)
    (append (getLightsRow firstRow) (getLightsRow secondRow))))

(define huesToCue
  (lambda (lightList)
    (cond
      ((null? lightList) (quote ()))
      ((eq? (car lightList) #t) (cons (length lightList) (huesToCue (cdr lightList))))
      (else (huesToCue (cdr lightList))))))

(define getHuesToCue
  (lambda (proc lst)
    (map (lambda (number)
           (- 17 number))
         (proc lst))))

(define lightList
  (lambda ()
    (getHuesToCue huesToCue lightsToCue)))

; Next is getting the lighting state for the cue.
(define lightingState '(0 1 0 0))

(define getOn
  (lambda (lst)
    (cond
      ((equal? 0 (car lst)) #t)
      (else #f))))

; We need to send the Cue to the Bridge.

(define bridgeResponse "")

(define goCue
  (lambda (lights state time)
    (for ([i (in-range (length lights))])
      (let-values ([(httpStatus httpHeader jsonResponse)
                    (http-sendrecv
                     bridgeAddress (string-append (string-append "/api/brucelighting/lights/" (number->string (list-ref lights i))) "/state")
                     #:method 'PUT
                     #:data
                     (jsexpr->string
                      (hash 'on (getOn state)
                            'bri (list-ref state 1)
                            'hue (list-ref state 2)
                            'sat (list-ref state 3)
                            'transitiontime cueTime))
                     #:headers
                     '("Content-Type: application/json")
                     #:content-decode '(json))])
        (set! bridgeResponse (read-json jsonResponse))))))

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
      ((= (list-ref state 0) 0) (set! lastOnMessage "On?: TRUE"))
      ((= (list-ref state 0) 1) (set! lastOnMessage "On?: FALSE"))
      (else (set! lastOnMessage "On? ")))
    (send lastOnDisplay set-label lastOnMessage)))

(define updateBri
  (lambda (state)
    (set! lastBriMessage (string-append "Intensity: " (number->string (list-ref state 1))))
    (send lastBriDisplay set-label lastBriMessage)))

(define updateHue
  (lambda (state)
    (set! lastHueMessage (string-append "Hue: " (number->string (list-ref state 2))))
    (send lastHueDisplay set-label lastHueMessage)))

(define updateSat
  (lambda (state)
    (set! lastSatMessage (string-append "Saturation: " (number->string (list-ref state 3))))
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

; We also need to be able to check on the state of all the lights.
; This is for "All Lights" Window. Pull data from the bridge.

(define updateAllLights
  (lambda (firstLight lastLight)
    (for ([i (in-range firstLight (+ lastLight 1))])
      (let-values ([(httpStatus httpHeader jsonResponse)
                    (http-sendrecv
                     bridgeAddress (string-append "/api/brucelighting/lights/" (number->string i))
                     #:method 'GET
                     #:headers
                     '("Content-Type: application/json")
                     #:content-decode '(json))])
        (let ([lightState (read-json jsonResponse)])
          (cond
            ((<= i 8)
             (cond
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #t)
                (send 
                 (list-ref (send (list-ref (send lights1To8 get-children) (- i 1)) get-children) 0) 
                 set-label 
                 (string-append initialOnMessage "T")))
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #f)
                (send 
                 (list-ref (send (list-ref (send lights1To8 get-children) (- i 1)) get-children) 0) 
                 set-label 
                 (string-append initialOnMessage "F"))))
             (send 
              (list-ref (send (list-ref (send lights1To8 get-children) (- i 1)) get-children) 1) 
              set-label 
              (string-append initialBriMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'bri))))
             (send 
              (list-ref (send (list-ref (send lights1To8 get-children) (- i 1)) get-children) 2) 
              set-label 
              (string-append initialHueMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'hue))))
             (send 
              (list-ref (send (list-ref (send lights1To8 get-children) (- i 1)) get-children) 3) 
              set-label 
              (string-append initialSatMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'sat)))))
            ((and (>= i 9) (<= i 16))
             (cond
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #t)
                (send 
                 (list-ref (send (list-ref (send lights9To16 get-children) (- i 9)) get-children) 0) 
                 set-label 
                 (string-append initialOnMessage "T")))
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #f)
                (send 
                 (list-ref (send (list-ref (send lights9To16 get-children) (- i 9)) get-children) 0) 
                 set-label 
                 (string-append initialOnMessage "F"))))
             (send 
              (list-ref (send (list-ref (send lights9To16 get-children) (- i 9)) get-children) 1) 
              set-label 
              (string-append initialBriMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'bri))))
             (send 
              (list-ref (send (list-ref (send lights9To16 get-children) (- i 9)) get-children) 2) 
              set-label 
              (string-append initialHueMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'hue))))
             (send 
              (list-ref (send (list-ref (send lights9To16 get-children) (- i 9)) get-children) 3) 
              set-label 
              (string-append initialSatMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'sat)))))))))))

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
                                                (set! lightingState (list 
                                                                     (send lightsChange get-selection) 
                                                                     (send lightsIntensity get-value) 
                                                                     (send lightsColor get-value) 
                                                                     (send lightsSaturation get-value))))]))

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

(define saveCueButtonPanel (new horizontal-panel% [parent saveCueDialog]
                                [alignment '(right center)]
                                [min-width 200]))

(define saveCueOKButton (new button% [parent saveCueButtonPanel]
                             [label "Save"]
                             [callback (lambda(button event)
                                         (let [(newCueName (send saveCueNameField get-value))]
                                           (send (new cue% [label newCueName]) set-parent mainList)
                                           (let [(newCuePosition (- (length (send mainList get-children)) 1))]
                                             (send (list-ref (send mainList get-children) newCuePosition) set-json (retrieveBridgeStatus)))
                                           (send cueChoice append newCueName)
                                           (send saveCueNameField set-value "")
                                           (send saveCueDialog show #f)))]))

(define saveCueCancelButton (new button% [parent saveCueButtonPanel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             (send saveCueNameField set-value "")
                                             (send saveCueDialog show #f))]))
; Create Go Button

(define cueGoPanel (new horizontal-panel% [parent cueGoAndSavePanel]
                        [alignment '(right center)]))

(define cueGoButton (new button% [parent cueGoPanel]
                         [label "GO!"]
                         [min-height 50]
                         [callback (lambda (button event)
                                     (goCue (lightList) lightingState cueTime)
                                     (updateLastStatus (lightList) lightingState cueTime)
                                     (updateAllLights 1 16))]))

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

(define initialOnMessage "On?: ")
(define initialBriMessage "Bri: ")
(define initialHueMessage "Hue: ")
(define initialSatMessage "Sat: ")

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

; Procedures for Saving, Restoring, and Deleting Cues.

(define retrieveBridgeStatus
  (lambda ()
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   bridgeAddress "/api/brucelighting/lights/"
                   #:method 'GET
                   #:headers
                   '("Content-Type: application/json")
                   #:content-decode '(json))])
      (read-json jsonResponse))))

(define getOneJsonState
  (lambda (cueList cueNumber lightNumber)
    (hash-ref 
     (hash-ref 
      (send 
       (list-ref (send cueList get-children) (- cueNumber 1)) 
       get-json) 
      (string->symbol (number->string lightNumber)))
     'state)))

; This procedure uses the last time set in the main control window.

(define restoreCue
  (lambda (cueList cueNumber numberOfLights)
    (for ([i (in-range 1 numberOfLights)])
      (let ([lightState (getOneJsonState cueList cueNumber i)])
        (let-values ([(httpStatus httpHeader jsonResponse)
                      (http-sendrecv
                       bridgeAddress (string-append (string-append "/api/brucelighting/lights/" (number->string i)) "/state")
                       #:method 'PUT
                       #:data
                       (jsexpr->string
                        (hash 'on (hash-ref lightState 'on)
                              'bri (hash-ref lightState 'bri)
                              'hue (hash-ref lightState 'hue)
                              'sat (hash-ref lightState 'sat)
                              'transitiontime cueTime))
                       #:headers
                       '("Content-Type: application/json")
                       #:content-decode '(json))])
          (set! bridgeResponse (read-json jsonResponse))
          (updateAllLights 1 16))))))

; The cue% object remains. I am unsure how to mark it for Garbage Collection.

(define deleteCue
  (lambda (cueList position)
    (let-values ([(cueList1 cueList2)
                  (split-at (send cueList get-children) position)])
      (send cueList set-children (append cueList1 (drop cueList2 1))))))

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
                                        (+ (send cueChoice get-selection) 1) 
                                        17))]))

<<<<<<< HEAD
; Create A Dialog for Saving Cues.

(define saveCueDialog (new dialog% [parent hueWindow]
                           [label "Save Cue"]))
(define saveCueNamePanel (new horizontal-panel% [parent saveCueDialog]
                              [alignment '(left center)]
                              [min-width 200]))
(define saveCueName (new text-field% [parent saveCueNamePanel]
                         [label "Cue Name:"]))

(define saveCueButtonPanel (new horizontal-panel% [parent saveCueDialog]
                                [alignment '(center center)]
                                [min-width 200]))

(define saveCueOKButton (new button% [parent saveCueButtonPanel]
                             [label "Save"]
                             [callback (lambda(button event)
                                         (let [(newCueName (send saveCueName get-value))]
                                           (send (new cue% [label newCueName]) set-parent mainList)
                                           (let [(newCuePosition (- (length (send mainList get-children)) 1))]
                                             (send (list-ref (send mainList get-children) newCuePosition) set-json (retrieveBridgeStatus)))
                                           (send cueChoice append newCueName)
                                           (send saveCueName set-value "")
                                           (send saveCueDialog show #f)))]))

(define saveCueCancelButton (new button% [parent saveCueButtonPanel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             (send saveCueName set-value "")
                                             (send saveCueDialog show #f))]))

; Menu Bars

; For Hue Window

(define hueWindowMenuBar (new menu-bar% [parent hueWindow]))

; File Menu
(define hueWindowMenuFile (new menu% [parent hueWindowMenuBar]
                               [label "File"]))
(define hueWindowMenuFileBridgeAddress (new menu-item% [parent hueWindowMenuFile]
                                      [label "Set Bridge Address…"]
                                      [callback (lambda (menu event)
                                                  (send bridgeAddressDialog show #t))]))
(define hueWindowMenuFileUserName (new menu-item% [parent hueWindowMenuFile]
                                      [label "Set User Name…"]
                                      [callback (lambda (menu event)
                                                  (send userNameDialog show #t))]))

; Set Bridge Address Dialog
(define bridgeAddressDialog (new dialog% [label "Enter Bridge Address"]
                                 [min-width 300]
                                 [min-height 100]))
(define bridgeAddressPanel (new horizontal-panel% [parent bridgeAddressDialog]
                                [alignment '(left center)]
                                [min-width 300]))
(define bridgeAddressField (new text-field% [parent bridgeAddressPanel]
                                [label "Bridge Address:"]
                                [init-value "0.0.0.0"]))
(define setBridgeAddressPanel (new horizontal-panel% [parent bridgeAddressDialog]
                                   [alignment '(center center)]
                                   [min-width 300]))
(define saveBridgeAddress (new button% [parent setBridgeAddressPanel]
                                 [label "Save"]
                                 [callback (lambda (button event)
                                             (set! bridgeAddress (send bridgeAddressField get-value))
                                             (send bridgeAddressDialog show #f))]))
(define cancelBridgeAddress (new button% [parent setBridgeAddressPanel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             (send bridgeAddressField set-value "0.0.0.0")
                                             (send bridgeAddressDialog show #f))]))

; Set Bridge Address Dialog
(define userNameDialog (new dialog% [label "Enter Hue Bridge User Name"]
                                 [min-width 300]
                                 [min-height 100]))
(define userNamePanel (new horizontal-panel% [parent userNameDialog]
                                [alignment '(left center)]
                                [min-width 300]))
(define userNameField (new text-field% [parent userNamePanel]
                                [label "User Name:"]))
(define setUserNamePanel (new horizontal-panel% [parent userNameDialog]
                                   [alignment '(center center)]
                                   [min-width 300]))
(define saveUserName (new button% [parent setUserNamePanel]
                                 [label "Save"]
                                 [callback (lambda (button event)
                                             (set! hueUserName (send userNameField get-value))
                                             (send userNameDialog show #f))]))
(define cancelUserName (new button% [parent setUserNamePanel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             (send userNameField set-value "")
                                             (send userNameDialog show #f))]))

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

=======
>>>>>>> Show_Control
; Show the Windows

(send statusWindow show #t)
(send allLights show #t)
(send cueListWindow show #t)
(send hueWindow show #t)