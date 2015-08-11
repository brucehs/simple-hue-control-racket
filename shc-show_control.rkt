#lang racket

(require net/http-client
         net/uri-codec
         json)

; Provide Main Light Adjustments.
(provide getLights
         lightList
         onChange?
         briChange?
         hueChange?
         satChange?
         on-pair
         bri-pair
         hue-pair
         sat-pair
         bridgeResponse
         onOrOff?
         makeJsonCommand
         bulbs-to-change
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

;; Provide lighting state comparisons.
(provide get-light-state
         compare-light-state
         wanted-state->list
         create-hash-for-bridge)

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

; Procedures to determine whether lighting variables have changed.
; There is an issue in which pressing the "Lighting State" button
; twice will set the change states to #f.

; TUDU: Create more sophisticated procedures that query the bridge
; directly. May need to create a light% class and a state field
; and instantiate an object for each light. This will allow
; patching as well.

(define onChange?
  (lambda (radio-box state)
    (cond
      ((equal?
        (onOrOff? (send radio-box get-selection))
        (hash-ref state 'on))
       (hash-set! state 'onChange #f))
      (else
       (hash-set! state 'on (onOrOff? (send radio-box get-selection)))
       (hash-set! state 'onChange #t)))))

(define briChange?
  (lambda (slider state)
    (cond
      ((equal? (send slider get-value) (hash-ref state 'bri))
       (hash-set! state 'briChange #f))
      (else
       (hash-set! state 'bri (send slider get-value))
       (hash-set! state 'briChange #t)))))

(define hueChange?
  (lambda (slider state)
    (cond
      ((equal? (send slider get-value) (hash-ref state 'hue))
       (hash-set! state 'hueChange #f))
      (else
       (hash-set! state 'hue (send slider get-value))
       (hash-set! state 'hueChange #t)))))

(define satChange?
  (lambda (slider state)
    (cond
      ((equal? (send slider get-value) (hash-ref state 'sat))
       (hash-set! state 'satChange #f))
      (else
       (hash-set! state 'sat (send slider get-value))
       (hash-set! state 'satChange #t)))))

(define on-pair
    (lambda (radio-box)
      (list (cons 'on (onOrOff? (send radio-box get-selection))))))

(define bri-pair
    (lambda (slider)
      (list (cons 'bri (send slider get-value)))))

(define hue-pair
    (lambda (slider)
      (list (cons 'hue (send slider get-value)))))

(define sat-pair
    (lambda (slider)
      (list (cons 'sat (send slider get-value)))))

; Procedures for creating a json command with just the variables
; that have changed.

(define hashForJson
  (lambda (state time)
    (make-hash
     (list
      (cond
        ((equal? (hash-ref state 'onChange) #t)
         (cons 'on (hash-ref state 'on)))
        (else
         '(() ())))
      (cond
        ((equal? (hash-ref state 'briChange) #t)
         (cons 'bri (hash-ref state 'bri)))
        (else
         '(() ())))
      (cond
        ((equal? (hash-ref state 'hueChange) #t)
         (cons 'hue (hash-ref state 'hue)))
        (else
         '(() ())))
      (cond
        ((equal? (hash-ref state 'satChange) #t)
         (cons 'sat (hash-ref state 'sat)))
        (else
         '(() ())))
      (cons 'transitiontime time)))))

;; TUDU, create a special circumastance if the only json command is
;; is "transitiontime" and abort the send.

(define makeJsonCommand
  (lambda (state time)
    (let ([hashCommand (hashForJson state time)])
      (cond
        ((hash-has-key? hashCommand '())
         (hash-remove! hashCommand '())))
      (jsexpr->string hashCommand))))

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
  (lambda (lights patch time address userName)
    (let ([bridgeResponse2 '()])
      (for ([i (in-range (length lights))])
        (let ([state
               (create-hash-for-bridge
                (list-ref
                 (send patch get-children)
                 (- (list-ref lights i) 1))
                (send (list-ref
                       (send patch get-children)
                       (- (list-ref lights i) 1))
                      get-bulb)
                address
                userName)])
        (let-values ([(httpStatus httpHeader jsonResponse)
                      (http-sendrecv
                       address (string-append 
                                (string-append 
                                 (string-append 
                                  (string-append "/api/" userName) 
                                  "/lights/") 
                                 (number->string
                                  (send (list-ref
                                         (send patch get-children)
                                         (- (list-ref lights i) 1))
                                         get-bulb))) 
                                "/state")
                       #:method 'PUT
                       #:data
                       (makeJsonCommand state time)
                       #:headers
                       '("Content-Type: application/json")
                       #:content-decode '(json))])
          (let ([response bridgeResponse2])
            (set! bridgeResponse2
                  (cons (read-json jsonResponse) response))))))
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

; TUDU. Compare json state of each light to the saved json state. Then compile
; json commands with just the changed values.

(define bridgeResponse
  (list ))

(define restoreCue
  (lambda (cueList cueNumber numberOfLights address userName)
    (let ([bridgeResponse2 '()])
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
            (let ([response bridgeResponse2])
              (set! bridgeResponse2
                    (cons (read-json jsonResponse) response))))))
      (set! bridgeResponse bridgeResponse2)
      (reverse bridgeResponse2))))

; I believe the cue% object remains. I am unsure how to mark it
; for Garbage Collection.

(define deleteCue
  (lambda (cueList position)
    (let-values ([(cueList1 cueList2)
                  (split-at (send cueList get-children) position)])
      (send cueList set-children (append cueList1 (drop cueList2 1))))
    (collect-garbage)))

; Procedures for comparing cues to current light state

(define get-light-state
  (lambda (lightNumber address userName)
    (hash-ref
     (hash-ref
      (retrieveBridgeStatus address userName)
      (string->symbol (number->string lightNumber)))
     'state)))

(define compare-light-state
  (lambda (hueObject lightNumber address userName)
    (make-hash
     (list
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'on)
                 (hash-ref (get-light-state lightNumber address userName) 'on))
         (cons 'onChange #f))
        (else
         (cons 'onChange #t)))
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'bri)
                 (hash-ref (get-light-state lightNumber address userName) 'bri))
         (cons 'briChange #f))
        (else
         (cons 'briChange #t)))
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'hue)
                 (hash-ref (get-light-state lightNumber address userName) 'hue))
         (cons 'hueChange #f))
        (else
         (cons 'hueChange #t)))
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'sat)
                 (hash-ref (get-light-state lightNumber address userName) 'sat))
         (cons 'satChange #f))
        (else
         (cons 'satChange #t)))))))

(define wanted-state->list
  (lambda (key hue-object)
    (list (cons key (hash-ref (send hue-object get-state) key)))))

(define create-hash-for-bridge
  (lambda (hue-object light-number address user-name)
    (make-hash
     (append
      (hash->list (compare-light-state
                   hue-object
                   light-number
                   address
                   user-name))
      (wanted-state->list 'on hue-object)
      (wanted-state->list 'bri hue-object)
      (wanted-state->list 'hue hue-object)
      (wanted-state->list 'sat hue-object)))))

(define bulbs-to-change
    (lambda (patch lights)
      (let ([bulb-list '()])
        (for ([i (in-range (length lights))])
            (set! bulb-list
                  (cons (send (list-ref (send patch get-children) (- (list-ref lights i) 1)) get-bulb) bulb-list)))
        (reverse bulb-list))))
