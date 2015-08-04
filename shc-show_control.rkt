#lang racket

(require net/http-client
         net/uri-codec
         json)

(provide bridgeResponse
         goLights
         retrieveBridgeStatus
         restoreCue
         deleteCue)

(define bridgeResponse
  (list ))

; Translates lightingState on/off value from 0 or 1 to #t or #f.

(define getOn
  (lambda (lst)
    (cond
      ((equal? 0 (car lst)) #t)
      (else #f))))

; Procedure for sending a a lighting state to the Bridge.
; Now requires a Bridge IP address and Bridge User Name variables,
; so can be used with multiple bridges in necessary.
; Returns a list with the bridge's response for each light.
; Not in the ideal fashion (it uses set!), but it works for the moment.
; I'm unsure of how to get the data from jsonResponse out of its
; local binding without using set!. It does not return from the function
; if called within the for loop.

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
                        (hash 'on (getOn state)
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
;(updateAllLights 1 16)))))) -- comment out until updateAllLights is moved to module

; I believe the cue% object remains. I am unsure how to mark it
; for Garbage Collection.

(define deleteCue
  (lambda (cueList position)
    (let-values ([(cueList1 cueList2)
                  (split-at (send cueList get-children) position)])
      (send cueList set-children (append cueList1 (drop cueList2 1))))
    (collect-garbage)))
