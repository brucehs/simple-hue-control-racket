#lang racket

(require net/http-client
         net/uri-codec
         json)

(define bridgeAddress "192.168.1.95")

(define hueUserName "brucelighting")

(define bridgeError "")

(define portalError "Error: Portal Connection Unavailable. Check the bridge's internet connection. Is the third light a steady blue?")

(define updatingError "Error: Bridge Is Currently Updating. Please Wait for Blue Lights to Return to Normal")

(define testResponse '(#hasheq((error
                                .
                                #hasheq((address . "/config/swupdate/checkforupdate")
                                        (type . 111)
                                        (description . "checkforupdate can only be set in updatestate 0 and 1"))))))

(define updateBridge
  (lambda ()
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
           (cond
             ((equal? (hash-ref (hash-ref (hash-ref bridgeResponse 'swupdate) 'devicetypes) 'bridge) #t)
              (cond
                ; Possible Responses:
                ; 0 = No update available
                ; 1 = Downloading updates
                ; 2 = Updates are ready to be installed
                ; 3 = Installing updates
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 0)
                 ; Set 'checkforupdate to #t.
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
                        (set! bridgeError (string-append "Error: " 
                                                         (hash-ref 
                                                          (hash-ref (car bridgeResponse2) 'error) 
                                                          'description))))
                       ((equal? (hash-keys (car bridgeResponse2)) '(success))
                        (set! bridgeError "Error: No Update Available. Will Check."))))))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 1)
                 (set! bridgeError "Error: Update Still Downloading. Please Wait."))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 2)
                 ; Need to initiate Update.
                 (let-values ([(httpStatus2 httpHeader2 jsonResponse2)
                               (http-sendrecv
                                bridgeAddress (string-append 
                                               (string-append "/api/" hueUserName) 
                                               "/config/") 
                                #:method 'PUT
                                #:data
                                (jsexpr->string
                                 (hash 'swupdate
                                       (hash 'updatestate 3)))
                                #:headers
                                '("Content-Type: application/json")
                                #:content-decode '(json))])
                   (let ([bridgeResponse2 (read-json jsonResponse2)])
                     (cond 
                       ((equal? (hash-keys (car bridgeResponse2)) '(error))
                        (set! bridgeError (string-append "Error: " 
                                                         (hash-ref 
                                                          (hash-ref (car bridgeResponse2) 'error) 
                                                          'description))))
                       ((equal? (hash-keys (car bridgeResponse2)) '(success))
                        (set! bridgeError ""))))))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 3)
                 (set! bridgeError updatingError))))
             ((equal? (hash-ref (hash-ref (hash-ref bridgeResponse 'swupdate) 'devicetypes) 'bridge) #f)
              (set! bridgeError "No Update Available."))))
          (else (set! bridgeError portalError)))))))