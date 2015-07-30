#lang racket
(define portalError "Error: Portal Connection Unavailable. Check the bridge's internet connection. Is the third light a steady blue?")

(define testResponse '(#hasheq((error
           .
           #hasheq((address . "/config/swupdate/checkforupdate")
                   (type . 111)
                   (description . "checkforupdate can only be set in updatestate 0 and 1"))))))

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
              (set! bridgeError (string-append "Error: " (hash-ref (hash-ref (car bridgeResponse2) 'error) 'description))))
             (else (set! bridgeError ""))))
      (else (set! bridgeError portalError)))))))