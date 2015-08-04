#lang racket

(require net/http-client
         net/uri-codec
         json)

; Provide Main Light Adjustments.
(provide getLights
         lightList
         bridgeResponse
         onOrOff?
         goLights)

; Provide Updating Light Status.
(provide initialOnMessage
         initialBriMessage
         initialHueMessage
         initialSatMessage
         updateAllLights)

; Provide Cue Manipulation.
(provide retrieveBridgeStatus
         restoreCue
         deleteCue)

; Procedures for translating selection in the "Select Lights to Cue" panel
; to data to be sent to the bridge.

(define getLightsRow 
  (lambda (panelContents)
    (cond
      ((null? panelContents) (quote ()))
      (else (cons
             (send (car panelContents) get-value)
             (getLightsRow (cdr panelContents)))))))

(define getLights 
  (lambda (firstRow secondRow)
    (append (getLightsRow firstRow) (getLightsRow secondRow))))

(define huesToCue
  (lambda (listOfLights)
    (cond
      ((null? listOfLights) (quote ()))
      ((eq? (car listOfLights) #t)
       (cons (length listOfLights) (huesToCue (cdr listOfLights))))
      (else (huesToCue (cdr listOfLights))))))

(define getHuesToCue
  (lambda (proc lst)
    (map (lambda (number)
           (- 17 number))
         (proc lst))))

(define lightList
  (lambda (whichLights)
    (getHuesToCue huesToCue whichLights)))

; Translates lightingState on/off value from 0 or 1 to #t or #f.

(define onOrOff?
  (lambda (state)
    (cond
      ((equal? state 0) #t)
      (else #f))))

; Procedure for sending a a lighting state to the Bridge.
; Now requires a Bridge IP address and Bridge User Name variables,
; so can be used with multiple bridges in necessary.
; Returns a list with the bridge's response for each light.
; Not in the ideal fashion (it uses set!), but it works for the moment.
; I'm unsure of how to get the data from jsonResponse out of its
; local binding without using set!. It does not return from the function
; if called within the for loop.

; TUDU, create a special circumstance when sending a command to all the lights
; that uses group 0.

(define goLights
  (lambda (lights state time address userName)
    (let ([bridgeResponse2 '()])
      (for ([i (in-range (length lights))])
        (let-values ([(httpStatus httpHeader jsonResponse)
                      (http-sendrecv
                       address (string-append 
                                (string-append 
                                 (string-append 
                                  (string-append "/api/" userName) 
                                  "/lights/") 
                                 (number->string (list-ref lights i))) 
                                "/state")
                       #:method 'PUT
                       #:data
                       (jsexpr->string
                        (hash 'on (list-ref state 0)
                              'bri (list-ref state 1)
                              'hue (list-ref state 2)
                              'sat (list-ref state 3)
                              'transitiontime time))
                       #:headers
                       '("Content-Type: application/json")
                       #:content-decode '(json))])
          (let ([response bridgeResponse2])
            (set! bridgeResponse2 (cons (read-json jsonResponse) response)))))
      (set! bridgeResponse bridgeResponse2)
      (reverse bridgeResponse2))))

; Procedure to update information about lighting state of every light.
; It pulls its data from the bridge.

(define initialOnMessage "On?: ")
(define initialBriMessage "Bri: ")
(define initialHueMessage "Hue: ")
(define initialSatMessage "Sat: ")

(define updateAllLights
  (lambda (firstLight lastLight lightLineOne lightLineTwo address userName)
    (for ([i (in-range firstLight (+ lastLight 1))])
      (let-values ([(httpStatus httpHeader jsonResponse)
                    (http-sendrecv
                     address (string-append 
                              (string-append 
                               (string-append "/api/" userName) 
                               "/lights/") 
                              (number->string i))
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
                 (list-ref (send
                            (list-ref (send lightLineOne get-children) (- i 1))
                            get-children)
                           0) 
                 set-label 
                 (string-append initialOnMessage "T")))
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #f)
                (send 
                 (list-ref (send
                            (list-ref (send lightLineOne get-children) (- i 1))
                            get-children)
                           0) 
                 set-label 
                 (string-append initialOnMessage "F"))))
             (send 
              (list-ref (send
                         (list-ref (send lightLineOne get-children) (- i 1))
                         get-children)
                        1) 
              set-label 
              (string-append initialBriMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'bri))))
             (send 
              (list-ref (send
                         (list-ref (send lightLineOne get-children) (- i 1))
                         get-children)
                        2) 
              set-label 
              (string-append initialHueMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'hue))))
             (send 
              (list-ref (send
                         (list-ref (send lightLineOne get-children) (- i 1))
                         get-children)
                        3) 
              set-label 
              (string-append initialSatMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'sat)))))
            ((and (>= i 9) (<= i 16))
             (cond
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #t)
                (send 
                 (list-ref (send
                            (list-ref (send lightLineTwo get-children) (- i 9))
                            get-children)
                           0) 
                 set-label 
                 (string-append initialOnMessage "T")))
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #f)
                (send 
                 (list-ref (send
                            (list-ref (send lightLineTwo get-children) (- i 9))
                            get-children)
                           0) 
                 set-label 
                 (string-append initialOnMessage "F"))))
             (send 
              (list-ref (send
                         (list-ref (send lightLineTwo get-children) (- i 9))
                         get-children)
                        1) 
              set-label 
              (string-append initialBriMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'bri))))
             (send 
              (list-ref (send
                         (list-ref (send lightLineTwo get-children) (- i 9))
                         get-children)
                        2) 
              set-label 
              (string-append initialHueMessage 
                             (number->string 
                              (hash-ref (hash-ref lightState 'state) 'hue))))
             (send 
              (list-ref (send
                         (list-ref (send lightLineTwo get-children) (- i 9))
                         get-children)
                        3) 
              set-label 
              (string-append initialSatMessage 
                             (number->string 
                              (hash-ref
                               (hash-ref lightState 'state)
                               'sat)))))))))))

; Procedures for Saving, Restoring, and Deleting Cues.

; Procedure for getting the current status of the bridge. This is used to
; save the current state as a cue.

(define retrieveBridgeStatus
  (lambda (address userName)
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   address (string-append
                            (string-append "/api/" userName)
                            "/lights/")
                   #:method 'GET
                   #:headers
                   '("Content-Type: application/json")
                   #:content-decode '(json))])
      (read-json jsonResponse))))

; Procedure used to iterate through saved json values to restore a saved cue.

(define getOneJsonState
  (lambda (cueList cueNumber lightNumber)
    (hash-ref 
     (hash-ref 
      (send 
       (list-ref (send cueList get-children) cueNumber) 
       get-json) 
      (string->symbol (number->string lightNumber)))
     'state)))

; Procedure for restoreing a saved cue.

; TUDU. Make procedure return values for all the lights in the cue.

(define bridgeResponse
  (list ))

(define restoreCue
  (lambda (cueList cueNumber numberOfLights address userName)
    (for ([i (in-range 1 numberOfLights)])
      (let ([lightState (getOneJsonState cueList cueNumber i)])
        (let-values ([(httpStatus httpHeader jsonResponse)
                      (http-sendrecv
                       address (string-append 
                                (string-append 
                                 (string-append 
                                  (string-append "/api/" userName) 
                                  "/lights/") 
                                 (number->string i)) 
                                "/state")
                       #:method 'PUT
                       #:data
                       (jsexpr->string
                        (hash 'on (hash-ref lightState 'on)
                              'bri (hash-ref lightState 'bri)
                              'hue (hash-ref lightState 'hue)
                              'sat (hash-ref lightState 'sat)
                              'transitiontime (send
                                               (list-ref
                                                (send cueList get-children)
                                                cueNumber)
                                               get-time)))
                       #:headers
                       '("Content-Type: application/json")
                       #:content-decode '(json))])
          (set! bridgeResponse (read-json jsonResponse)))))))

; I believe the cue% object remains. I am unsure how to mark it
; for Garbage Collection.

(define deleteCue
  (lambda (cueList position)
    (let-values ([(cueList1 cueList2)
                  (split-at (send cueList get-children) position)])
      (send cueList set-children (append cueList1 (drop cueList2 1))))
    (collect-garbage)))
