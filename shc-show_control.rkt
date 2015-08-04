#lang racket

(require net/http-client
         net/uri-codec
         json)

(provide bridgeResponse
         goLights)

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
        (let ([response bridgeResponse])
        (set! bridgeResponse (cons (read-json jsonResponse) response)))))
    (reverse bridgeResponse)))