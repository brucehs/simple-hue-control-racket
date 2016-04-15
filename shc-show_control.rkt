#lang racket

(require net/http-client
         net/uri-codec
         json)

;; Provide Main Light Adjustments.
(provide get-lights
         lightList
         on-pair
         bri-pair
         hue-pair
         sat-pair
         set-lights!)

;; Provide Updating Light Status.
(provide initialOnMessage
         initialBriMessage
         initialHueMessage
         initialSatMessage
         update-all-lights)

;; Provide Cue Manipulation.
(provide retrieveBridgeStatus
         restore-cue
         delete-cue
         resort-cue-choice)

; Procedures for translating selection in the "Select Lights to Cue" panel
; to data to be sent to the bridge.

(define get-lights-row 
  (lambda (panel-contents)
    (cond
      ((null? panel-contents) (quote ()))
      (else (cons
             (send (car panel-contents) get-value)
             (get-lights-row (cdr panel-contents)))))))

(define get-lights 
  (lambda (first-row second-row)
    (append (get-lights-row first-row) (get-lights-row second-row))))

(define hues-to-change
  (lambda (lst-of-lights)
    (cond
      ((null? lst-of-lights) (quote ()))
      ((eq? (car lst-of-lights) #t)
       (cons (length lst-of-lights) (hues-to-change (cdr lst-of-lights))))
      (else (hues-to-change (cdr lst-of-lights))))))

(define get-hues-to-change
  (lambda (proc lst)
    (map (lambda (number)
           (- 17 number))
         (proc lst))))

(define lightList
  (lambda (which-lights)
    (get-hues-to-change hues-to-change which-lights)))

;; Translates lightingState on/off value from 0 or 1 to #t or #f.

(define on-or-off?
  (lambda (state)
    (cond
      ((equal? state 0) #t)
      (else #f))))

;; Procedures to assign lighting states to light% objects.

(define on-pair
  (lambda (radio-box)
    (list (cons 'on (on-or-off? (send radio-box get-selection))))))

(define bri-pair
  (lambda (slider)
    (list (cons 'bri (send slider get-value)))))

(define hue-pair
  (lambda (slider)
    (list (cons 'hue (send slider get-value)))))

(define sat-pair
  (lambda (slider)
    (list (cons 'sat (send slider get-value)))))

;; Procedures for comparing light% object states to their bulbs
;; state in the bridge.

(define get-light-state
  (lambda (lightNumber address user-name)
    (hash-ref
     (hash-ref
      (retrieveBridgeStatus address user-name)
      (string->symbol (number->string lightNumber)))
     'state)))

;; Procedures for direct change.

(define compare-light-state
  (lambda (hueObject lightNumber address user-name)
    (make-immutable-hash
     (list
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'on)
                 (hash-ref (get-light-state lightNumber address user-name) 'on))
         (cons 'onChange #f))
        (else
         (cons 'onChange #t)))
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'bri)
                 (hash-ref (get-light-state lightNumber address user-name) 'bri))
         (cons 'briChange #f))
        (else
         (cons 'briChange #t)))
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'hue)
                 (hash-ref (get-light-state lightNumber address user-name) 'hue))
         (cons 'hueChange #f))
        (else
         (cons 'hueChange #t)))
      (cond
        ((equal? (hash-ref (send hueObject get-state) 'sat)
                 (hash-ref (get-light-state lightNumber address user-name) 'sat))
         (cons 'satChange #f))
        (else
         (cons 'satChange #t)))))))

(define wanted-state->list
  (lambda (key hue-object)
    (list (cons key (hash-ref (send hue-object get-state) key)))))

(define create-hash-for-bridge
  (lambda (hue-object light-number address user-name)
    (make-immutable-hash
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

;; Procedure for Restoring Cue

(define compare-light-state-in-cue
  (lambda (cue-list cue-number light-number address user-name)
    (let ([cue-light-state
           (hash-ref
            (hash-ref
             (send
              (list-ref
               (send cue-list get-children)
               cue-number)
              get-json)
             (string->symbol (number->string light-number)))
            'state)])
      (make-immutable-hash
       (list
        (cond
          ((equal? (hash-ref cue-light-state 'on)
                   (hash-ref
                    (get-light-state light-number address user-name)
                    'on))
           (cons 'onChange #f))
          (else
           (cons 'onChange #t)))
        (cond
          ((equal? (hash-ref cue-light-state 'bri)
                   (hash-ref
                    (get-light-state light-number address user-name)
                    'bri))
           (cons 'briChange #f))
          (else
           (cons 'briChange #t)))
        (cond
          ((equal? (hash-ref cue-light-state 'hue)
                   (hash-ref
                    (get-light-state light-number address user-name)
                    'hue))
           (cons 'hueChange #f))
          (else
           (cons 'hueChange #t)))
        (cond
          ((equal? (hash-ref cue-light-state 'sat)
                   (hash-ref
                    (get-light-state light-number address user-name)
                    'sat))
           (cons 'satChange #f))
          (else
           (cons 'satChange #t))))))))

(define create-restore-hash-for-bridge
  (lambda (cue-list cue-number light-number address user-name)
    (let ([cue-light-state
           (hash-ref
            (hash-ref
             (send
              (list-ref
               (send cue-list get-children)
               cue-number)
              get-json)
             (string->symbol (number->string light-number)))
            'state)])
      (make-immutable-hash
       (append
        (hash->list (compare-light-state-in-cue
                     cue-list
                     cue-number
                     light-number
                     address
                     user-name))
        (list (cons 'on (hash-ref cue-light-state 'on)))
        (list (cons 'bri (hash-ref cue-light-state 'bri)))
        (list (cons 'hue (hash-ref cue-light-state 'hue)))
        (list (cons 'sat (hash-ref cue-light-state 'sat))))))))

; Procedures for creating a json command with just the variables
; that have changed.

(define hashForJson
  (lambda (state time)
    (make-immutable-hash
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

;; Procedure for sending a a lighting state to the Bridge.
;; Now requires a Bridge IP address and Bridge User Name variables,
;; so can be used with multiple bridges in necessary.
;; Returns a list with the bridge's response for each light.

(define set-lights!
  (lambda (lights patch time address user-name)
    (cond
      ((and (equal? (length lights) 16)
            (<= time 10))
       (let ([state
              (make-immutable-hash
               (cons
                (cons 'transitiontime time)
                (hash->list (send (list-ref (send patch get-children) 0) get-state))))])
         (let-values ([(httpStatus httpHeader jsonResponse)
                       (http-sendrecv
                        address  (string-append 
                                  (string-append "/api/" user-name) 
                                  "/groups/0/action") 
                        #:method 'PUT
                        #:data
                        (jsexpr->string state)
                        #:headers
                        '("Content-Type: application/json")
                        #:content-decode '(json))])
           (read-json jsonResponse))))
      (else
       (for/list ([i (in-range (length lights))])
         (when (> i 1) (sleep .01))
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
                 user-name)])
           (let-values ([(httpStatus httpHeader jsonResponse)
                         (http-sendrecv
                          address (string-append 
                                   (string-append 
                                    (string-append 
                                     (string-append "/api/" user-name) 
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
             (read-json jsonResponse))))))))

; Procedure to update information about lighting state of every light.
; It pulls its data from the bridge.

(define initialOnMessage "On?: ")
(define initialBriMessage "Bri: ")
(define initialHueMessage "Hue: ")
(define initialSatMessage "Sat: ")

(define update-all-lights
  (lambda (firstLight lastLight lightLineOne lightLineTwo address user-name)
    (for/list ([i (in-range firstLight (+ lastLight 1))])
      (let-values ([(httpStatus httpHeader jsonResponse)
                    (http-sendrecv
                     address (string-append 
                              (string-append 
                               (string-append "/api/" user-name) 
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
  (lambda (address user-name)
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   address (string-append
                            (string-append "/api/" user-name)
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

(define restore-cue
  (lambda (cueList cueNumber numberOfLights address user-name)
    (for/list ([i (in-range 1 numberOfLights)])
      (when (> i 1) (sleep .01))
      (let ([lightState
             (create-restore-hash-for-bridge
              cueList
              cueNumber
              i
              address
              user-name)])
        (let ([time
               (send 
                (list-ref
                 (send cueList get-children)
                 cueNumber)
                get-time)])
          (let-values ([(httpStatus httpHeader jsonResponse)
                        (http-sendrecv
                         address (string-append 
                                  (string-append 
                                   (string-append 
                                    (string-append "/api/" user-name) 
                                    "/lights/") 
                                   (number->string i)) 
                                  "/state")
                         #:method 'PUT
                         #:data
                         (makeJsonCommand lightState time)
                         #:headers
                         '("Content-Type: application/json")
                         #:content-decode '(json))])
            (read-json jsonResponse)))))))

; I believe the cue% object remains. I am unsure how to mark it
; for Garbage Collection.

(define delete-cue
  (lambda (cueList position)
    (let-values ([(cueList1 cueList2)
                  (split-at (send cueList get-children) position)])
      (send cueList set-children (append cueList1 (drop cueList2 1))))
    (collect-garbage)))

; Procedures for resorting the cue list upon saving cues out of order.

(define get-cue-strings
  (lambda (cue-choice)
    (for/list ([i (in-range (send cue-choice get-number))])
      (send cue-choice get-string i))))

(define extract-cue-number-from-string
  (lambda (cue-str)
    (let ([cue-number (list-ref (string-split cue-str) 0)])
      (string->number (list-ref (string-split cue-number ".") 0)))))

(define get-list-of-cue-numbers-from-choice
    (lambda (cue-choice)
      (for/list ([i (in-range (send cue-choice get-number))])
        (extract-cue-number-from-string (send cue-choice get-string i)))))

(define compare-cue-numbers
  (lambda (cue-str cue-obj)
    (let ([cue-str-number
           (extract-cue-number-from-string cue-str)]
          [cue-obj-number
           (send cue-obj get-number)])
      (cond
        ((= cue-str-number cue-obj-number) #t)
        (else #f)))))

(define hash-cue-numbers-and-cue-strings
  (lambda (cue-choice)
    (for/hash ([i (in-range (length (get-cue-strings cue-choice)))])
      (values
       (list-ref (get-list-of-cue-numbers-from-choice cue-choice) i)
       (list-ref (get-cue-strings cue-choice) i)))))

(define resort-list-of-cue-strings
  (lambda (cue-choice)
    (let ([cues-hash (hash-cue-numbers-and-cue-strings cue-choice)]
          [cues-key (sort (hash-keys (hash-cue-numbers-and-cue-strings cue-choice)) <)])
      (for/list ([i (in-range (length cues-key))])
        (hash-ref cues-hash (list-ref cues-key i))))))

(define resort-cue-choice
  (lambda (cue-choice)
    (let ([list-of-cue-strings
           (resort-list-of-cue-strings cue-choice)])
      (send cue-choice clear)
      (for ([i (in-range (length list-of-cue-strings))])
        (send cue-choice append (list-ref list-of-cue-strings i))))))