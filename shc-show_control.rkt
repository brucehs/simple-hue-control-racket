#lang racket

(require net/http-client
         net/uri-codec
         json
         "shc-classes.rkt")

;; Provide Main Light Adjustments.
(provide get-lights
         get-attributes
         lightList
         on-pair
         bri-pair
         hue-pair
         sat-pair
         set-lights!)

;; Provide Updating Light Status.
(provide initial-on-message
         initial-bri-message
         initial-hue-message
         initial-sat-message
         update-all-lights)

;; Provide Cue Manipulation.
(provide retrieve-bridge-status
         restore-cue
         delete-cue
         resort-cue-choice
         resort-cue-list)

; Procedures for translating selection in the "Select Lights to Cue" panel
; to data to be sent to the bridge.

(define get-lights
  (lambda (panel-top panel-bottom)
    (append
     (let ([object-lst (send panel-top get-children)])
       (for/list ([i (in-range (length object-lst))])
         (send (list-ref object-lst i) get-value)))
     (let ([object-lst (send panel-bottom get-children)])
       (for/list ([i (in-range (length object-lst))])
         (send (list-ref object-lst i) get-value))))))

(define get-attributes
  (lambda (top-panel
           bottom-panel
           lights-on?
           lights-intensity
           lights-color
           lights-saturation)
    (let ([light-objects (lightList (get-lights top-panel bottom-panel))])
      (for ([i (in-range (length light-objects))])
        (send
         (list-ref (send primary-patch get-children) (- (list-ref light-objects i) 1))
         set-state
         (make-hash
          (append
           (on-pair lights-on?)
           (bri-pair lights-intensity)
           (hue-pair lights-color)
           (sat-pair lights-saturation))))))))

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
  (lambda (bridge-status light-number)
    (hash-ref
     (hash-ref
      bridge-status
      (string->symbol (number->string light-number)))
     'state)))

;; Procedures for direct change.

(define compare-light-state
  (lambda (hue-object bridge-status light-number)
    (make-immutable-hash
     (list
      (cond
        ((equal? (hash-ref (send hue-object get-state) 'on)
                 (hash-ref (get-light-state bridge-status light-number) 'on))
         (cons 'onChange #f))
        (else
         (cons 'onChange #t)))
      (cond
        ((equal? (hash-ref (send hue-object get-state) 'bri)
                 (hash-ref (get-light-state bridge-status light-number) 'bri))
         (cons 'briChange #f))
        (else
         (cons 'briChange #t)))
      (cond
        ((equal? (hash-ref (send hue-object get-state) 'hue)
                 (hash-ref (get-light-state bridge-status light-number) 'hue))
         (cons 'hueChange #f))
        (else
         (cons 'hueChange #t)))
      (cond
        ((equal? (hash-ref (send hue-object get-state) 'sat)
                 (hash-ref (get-light-state bridge-status light-number) 'sat))
         (cons 'satChange #f))
        (else
         (cons 'satChange #t)))))))

(define wanted-state->list
  (lambda (key hue-object)
    (list (cons key (hash-ref (send hue-object get-state) key)))))

(define create-hash-for-bridge
  (lambda (hue-object bridge-status light-number)
    (make-immutable-hash
     (append
      (hash->list (compare-light-state hue-object bridge-status light-number))
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
             (send (list-ref (send cue-list get-children) cue-number) get-json)
             (string->symbol (number->string light-number)))
            'state)])
      (make-hash
       (list
        (cond
          ((equal?
            (hash-ref cue-light-state 'on)
            (hash-ref (get-light-state light-number address user-name) 'on))
           (cons 'onChange #f))
          (else (cons 'onChange #t)))
        (cond
          ((equal?
            (hash-ref cue-light-state 'bri)
            (hash-ref (get-light-state light-number address user-name) 'bri))
           (cons 'briChange #f))
          (else (cons 'briChange #t)))
        (cond
          ((equal?
            (hash-ref cue-light-state 'hue)
            (hash-ref (get-light-state light-number address user-name) 'hue))
           (cons 'hueChange #f))
          (else (cons 'hueChange #t)))
        (cond
          ((equal?
            (hash-ref cue-light-state 'sat)
            (hash-ref (get-light-state light-number address user-name)'sat))
           (cons 'satChange #f))
          (else (cons 'satChange #t))))))))

(define create-restore-hash-for-bridge
  (lambda (cue-list cue-number light-number address user-name)
    (let ([cue-light-state
           (hash-ref
            (hash-ref
             (send (list-ref (send cue-list get-children) cue-number) get-json)
             (string->symbol (number->string light-number)))
            'state)])
      (make-hash
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
    (make-hash
     (list
      (cond
        ((equal? (hash-ref state 'onChange) #t) (cons 'on (hash-ref state 'on)))
        (else '(() ())))
      (cond
        ((equal? (hash-ref state 'briChange) #t) (cons 'bri (hash-ref state 'bri)))
        (else '(() ())))
      (cond
        ((equal? (hash-ref state 'hueChange) #t) (cons 'hue (hash-ref state 'hue)))
        (else '(() ())))
      (cond
        ((equal? (hash-ref state 'satChange) #t) (cons 'sat (hash-ref state 'sat)))
        (else '(() ())))
      (cons 'transitiontime time)))))

;; TUDU, create a special circumastance if the only json command is
;; is "transitiontime" and abort the send.

(define makeJsonCommand
  (lambda (state time)
    (let ([hashCommand (hashForJson state time)])
      (cond ((hash-has-key? hashCommand '()) (hash-remove! hashCommand '())))
      (jsexpr->string hashCommand))))

;; Procedure for sending a a lighting state to the Bridge.
;; Now requires a Bridge IP address and Bridge User Name variables,
;; so can be used with multiple bridges in necessary.
;; Returns a list with the bridge's response for each light.

(define set-lights!
  (lambda (lights patch time address user-name)
    (cond
      ((and (equal? (length lights) 16) (<= time 10))
       (let ([state
              (make-hash
               (cons
                (cons 'transitiontime time)
                (hash->list (send (list-ref (send patch get-children) 0) get-state))))])
         (let-values ([(httpStatus httpHeader jsonResponse)
                       (http-sendrecv
                        address  (string-append "/api/" user-name "/groups/0/action") 
                        #:method 'PUT
                        #:data
                        (jsexpr->string state)
                        #:headers
                        '("Content-Type: application/json")
                        #:content-decode '(json))])
           (read-json jsonResponse))))
      (else
       (let ([bridge-status (retrieve-bridge-status address user-name)])
       (for/list ([i (in-range (length lights))])
         (when (> i 1) (sleep .01))
         (let ([state
                (create-hash-for-bridge
                 (list-ref
                  (send patch get-children)
                  (- (list-ref lights i) 1))
                 bridge-status
                 (send (list-ref
                        (send patch get-children)
                        (- (list-ref lights i) 1))
                       get-bulb))])
           (let-values ([(httpStatus httpHeader jsonResponse)
                         (http-sendrecv
                          address (string-append
                                   "/api/"
                                   user-name
                                   "/lights/" 
                                   (number->string
                                    (send
                                     (list-ref (send patch get-children) (- (list-ref lights i) 1))
                                     get-bulb))
                                   "/state")
                          #:method 'PUT
                          #:data
                          (makeJsonCommand state time)
                          #:headers
                          '("Content-Type: application/json")
                          #:content-decode '(json))])
             (read-json jsonResponse)))))))))

; Procedure to update information about lighting state of every light.
; It pulls its data from the bridge.

(define initial-on-message "On?: ")
(define initial-bri-message "Bri: ")
(define initial-hue-message "Hue: ")
(define initial-sat-message "Sat: ")

(define update-all-lights
  (lambda (firstLight lastLight lightLineOne lightLineTwo address user-name)
    (for/list ([i (in-range firstLight (+ lastLight 1))])
      (let-values ([(httpStatus httpHeader jsonResponse)
                    (http-sendrecv
                     address (string-append "/api/" user-name "/lights/" (number->string i))
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
                 (list-ref
                  (send (list-ref (send lightLineOne get-children) (- i 1)) get-children)
                  0) 
                 set-label 
                 (string-append initial-on-message "T")))
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #f)
                (send 
                 (list-ref
                  (send (list-ref (send lightLineOne get-children) (- i 1)) get-children)
                  0) 
                 set-label 
                 (string-append initial-on-message "F"))))
             (send 
              (list-ref
               (send (list-ref (send lightLineOne get-children) (- i 1)) get-children)
               1) 
              set-label 
              (string-append
               initial-bri-message
               (number->string (hash-ref (hash-ref lightState 'state) 'bri))))
             (send 
              (list-ref
               (send (list-ref (send lightLineOne get-children) (- i 1)) get-children)
               2) 
              set-label 
              (string-append
               initial-hue-message
               (number->string (hash-ref (hash-ref lightState 'state) 'hue))))
             (send 
              (list-ref
               (send (list-ref (send lightLineOne get-children) (- i 1)) get-children)
               3) 
              set-label 
              (string-append
               initial-sat-message
               (number->string (hash-ref (hash-ref lightState 'state) 'sat)))))
            ((and (>= i 9) (<= i 16))
             (cond
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #t)
                (send 
                 (list-ref (send (list-ref (send lightLineTwo get-children) (- i 9)) get-children)
                           0) 
                 set-label 
                 (string-append initial-on-message "T")))
               ((eq? (hash-ref (hash-ref lightState 'state) 'on) #f)
                (send 
                 (list-ref
                  (send (list-ref (send lightLineTwo get-children) (- i 9)) get-children)
                  0) 
                 set-label 
                 (string-append initial-on-message "F"))))
             (send 
              (list-ref
               (send (list-ref (send lightLineTwo get-children) (- i 9)) get-children)
               1) 
              set-label 
              (string-append
               initial-bri-message 
               (number->string (hash-ref (hash-ref lightState 'state) 'bri))))
             (send 
              (list-ref
               (send (list-ref (send lightLineTwo get-children) (- i 9)) get-children)
               2) 
              set-label 
              (string-append
               initial-hue-message 
               (number->string (hash-ref (hash-ref lightState 'state) 'hue))))
             (send 
              (list-ref
               (send (list-ref (send lightLineTwo get-children) (- i 9)) get-children)
               3) 
              set-label 
              (string-append
               initial-sat-message
               (number->string (hash-ref (hash-ref lightState 'state) 'sat)))))))))))

; Procedures for Saving, Restoring, and Deleting Cues.

; Procedure for getting the current status of the bridge. This is used to
; save the current state as a cue.

(define retrieve-bridge-status
  (lambda (address user-name)
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   address (string-append "/api/" user-name "/lights/")
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
  (lambda (cue-list cue-number number-of-lights address user-name)
    (for/list ([i (in-range 1 number-of-lights)])
      (when (> i 1) (sleep .01))
      (let ([lightState
             (create-restore-hash-for-bridge
              cue-list
              cue-number
              i
              address
              user-name)])
        (let ([time (send (list-ref (send cue-list get-children) cue-number) get-time)])
          (let-values ([(httpStatus httpHeader jsonResponse)
                        (http-sendrecv
                         address (string-append
                                  "/api/"
                                  user-name
                                  "/lights/"
                                  (number->string i)
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
  (lambda (cue-list position)
    (let-values ([(cue-list-1 cue-list-2)
                  (split-at (send cue-list get-children) position)])
      (send cue-list set-children (append cue-list-1 (drop cue-list-2 1))))
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

; Creates a hash with the keys being the cue numbers and the values
; being the cue-choice strings.

(define hash-cue-numbers-and-cue-strings
  (lambda (cue-choice)
    (for/hash ([i (in-range (length (get-cue-strings cue-choice)))])
      (values
       (list-ref (get-list-of-cue-numbers-from-choice cue-choice) i)
       (list-ref (get-cue-strings cue-choice) i)))))

; Creates a list with the cue strings in proper numerical order.

(define resort-list-of-cue-strings
  (lambda (cue-choice)
    (let ([cues-hash
           (hash-cue-numbers-and-cue-strings cue-choice)]
          [cues-key
           (sort (hash-keys (hash-cue-numbers-and-cue-strings cue-choice)) <)])
      (for/list ([i (in-range (length cues-key))])
        (hash-ref cues-hash (list-ref cues-key i))))))

; Actually resorts the cue-choice choice% popup menu.

(define resort-cue-choice
  (lambda (cue-choice)
    (let ([list-of-cue-strings
           (resort-list-of-cue-strings cue-choice)])
      (send cue-choice clear)
      (for ([i (in-range (length list-of-cue-strings))])
        (send cue-choice append (list-ref list-of-cue-strings i))))))

; Procedures for resorting a cue-list% object

(define compare-cue-number
  (lambda (cue-choice-number cue-obj)
    (let ([cue-obj-number
           (send cue-obj get-number)])
      (cond
        ((= cue-choice-number cue-obj-number) #t)
        (else #f)))))

(define return-cue-object
  (lambda (lst)
    (cond
      ((object? (car lst))
       (car lst))
      ((null? (cdr lst))
       (error "no object"))
      (else (return-cue-object (cdr lst))))))

(define get-cue-object-for-hash
  (lambda (cue-choice index cue-list)
    (let ([list-of-cue-numbers
           (get-list-of-cue-numbers-from-choice cue-choice)])
      (return-cue-object
       (for/list ([i (in-range (length list-of-cue-numbers))])
         (cond
           ((compare-cue-number
             (list-ref list-of-cue-numbers index)
             (list-ref (send cue-list get-children) i))
            (list-ref (send cue-list get-children) i))
           (else #f)))))))

(define hash-cue-numbers-and-cue-objects
  (lambda (cue-choice cue-list)
    (let ([list-of-cue-numbers
           (get-list-of-cue-numbers-from-choice cue-choice)])
      (for/hash ([i (in-range (length list-of-cue-numbers))])
        (values
         (list-ref list-of-cue-numbers i)
         (get-cue-object-for-hash cue-choice i cue-list))))))

(define resort-list-of-cue-objects
  (lambda (cue-choice cue-list)
    (let ([cues-hash
           (hash-cue-numbers-and-cue-objects cue-choice cue-list)]
          [cues-key
           (sort (hash-keys (hash-cue-numbers-and-cue-objects cue-choice cue-list)) <)])
      (for/list ([i (in-range (length cues-key))])
        (hash-ref cues-hash (list-ref cues-key i))))))

(define resort-cue-list
  (lambda (cue-choice cue-list)
    (let ([new-cue-order
           (resort-list-of-cue-objects cue-choice cue-list)])
      (send cue-list set-children new-cue-order))))
