#lang racket/gui

(require net/http-client
         net/uri-codec
         json)

(compile-allow-set!-undefined #t)

; Variables for Future Use.

(define bridgeAddress "192.168.1.95")

; We begin by creating the procedures we will need.

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

; We need to read the response from the Bridge.

(define onKey '/lights/1/state/on) 
(define briKey '/lights/1/state/bri)
(define hueKey '/lights/1/state/hue)
(define satKey '/lights/1/state/sat)
(define transitiontimeKey '/lights/1/state/transitiontime)

(define bridgeOn #f)
(define bridgeBri 1)
(define bridgeHue 0)
(define bridgeSat 0)
(define bridgeTransitiontime 0)

(define setResponseValues
  (lambda (response)
    (let ([numberOfValues (length response)])
      (for ([i (in-range 0 numberOfValues)])
        (cond
          ((hash-has-key? (car (hash-values (list-ref response i))) onKey)
           (set! bridgeOn (hash-ref (car (hash-values (list-ref response i))) onKey)))
          ((hash-has-key? (car (hash-values (list-ref response i))) briKey)
           (set! bridgeBri (hash-ref (car (hash-values (list-ref response i))) briKey)))
          ((hash-has-key? (car (hash-values (list-ref response i))) hueKey)
           (set! bridgeHue (hash-ref (car (hash-values (list-ref response i))) hueKey)))
          ((hash-has-key? (car (hash-values (list-ref response i))) satKey)
           (set! bridgeSat (hash-ref (car (hash-values (list-ref response i))) satKey)))
          ((hash-has-key? (car (hash-values (list-ref response i))) transitiontimeKey)
           (cond
             ((equal? (hash-ref (car (hash-values (list-ref response i))) transitiontimeKey) 0)
              (set! bridgeTransitiontime 0))
             (else (set! bridgeTransitiontime (/ (hash-ref (car (hash-values (list-ref response i))) transitiontimeKey) 10))))))))))

; We also need to be able to check on the state of all the lights.

;(define light1OnState #f)
;(define light1BriState 1)
;(define light1HueState 0)
;(define light1SatState 0)
;
;(define getStateOfAllLights
;  (lambda (response)
;    (let ([numberOfValues (hash-count response)])
;      (for ([lightKey (in-range 0 numberOfValues)])
;        (cond
;          ((hash-has-key? response (string->symbol (number->string lightKey)))
;           (set! light1OnState (hash-ref (hash-ref (hash-ref response (string->symbol (number->string lightKey))) 'state) 'on))
;           (set! light1BriState (hash-ref (hash-ref (hash-ref response (string->symbol (number->string lightKey))) 'state) 'bri))
;           (set! light1HueState (hash-ref (hash-ref (hash-ref response (string->symbol (number->string lightKey))) 'state) 'hue))
;           (set! light1SatState (hash-ref (hash-ref (hash-ref response (string->symbol (number->string lightKey))) 'state) 'sat))))))))

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

; Now it is time to create a window.

(define hueWindow (new frame% [label "Simple Hue Control"]))

; Next we create some panels to order the window.

; First is Selecting the Lights to Cue.
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
; Create some check boxes to select lights
(for ([i (in-range 1 9)])
  (new check-box% [parent lightsSelectPanelTop]
       [label (string-append "LX " (string-append (~a i) " "))]
       [value #f]))
(for ([i (in-range 9 17)])
  (new check-box% [parent lightsSelectPanelBottom]
       [label (string-append "LX " (string-append (~a i) " "))]
       [value #f]))

(define lightsSelectButton (new button% [parent lightsSelect]
                                [label "Lights!"]
                                [callback (lambda (button event)
                                            (set! lightsToCue (getLights
                                                               (send lightsSelectPanelTop get-children)
                                                               (send lightsSelectPanelBottom get-children))))]))

; Next is Setting the Attributes
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

(define lightsAttributesButton (new button% [parent lightsAttributes]
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
                                          (set! cueTime
                                                (inexact->exact (* (string->number (send cueTimeField get-value)) 10))))]))

; Now we need to send the cue to the bridge.

(define cueGoPanel (new horizontal-panel% [parent hueWindow]
                        [style '(border)]
                        [alignment '(right center)]))

; The following needs a callback procedure.
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

; Display the last cue.

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

; Finally we need a window to show the status of all the lights

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

; Show the Windows

(send hueWindow show #t)
(send statusWindow show #t)
(send allLights show #t)

; Will Need

;(define-values (httpStatus httpHeader jsonResponse) (http-sendrecv
; bridgeAddress "/api/brucelighting/lights/1/state"
; #:method 'PUT
; #:data
; (jsexpr->string 
;  (hash 'on #t
;        'transitiontime 50
;        'bri 250
;        'ct 300)) 
; #:headers
; '("Content-Type: application/json")
; #:content-decode '(json)))
;
;(define-values (httpStatus httpHeader jsonResponse) (http-sendrecv
; bridgeAddress "/api/brucelighting/lights/"
; #:method 'GET
; #:headers
; '("Content-Type: application/json")
; #:content-decode '(json)))
;
;(define bridgeResponse (read-json jsonResponse))

;(goCue (lightList) lightingState cueTime)