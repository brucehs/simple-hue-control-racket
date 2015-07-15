#lang racket

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
                  (set! bridgeResponse (read-json jsonResponse)))))))