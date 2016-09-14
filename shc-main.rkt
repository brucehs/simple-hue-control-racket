#lang racket/gui

(require framework
         net/http-client
         net/uri-codec
         json
         "shc-classes.rkt"
         "shc-gui.rkt"
         "shc-save_load.rkt"
         "shc-settings.rkt"
         "shc-show_control.rkt")

(compile-allow-set!-undefined #t)

;; Creates a folder in ~/Library/Application Support/ if one does not exist.
;; Create Bridge Address and User Name files if they do not exist. Otherwise,
;; open the files.

(support-directory-exists?)

(define setup-needed
  (not (bridge-settings-file-exists?)))


;; Bridge Communication Variables. Communication will not work until 
;; these are set by the user.

;; Need to set up error handling if the user tries to use the application
;; before setting these.

(define bridge-address (hash-ref (file->value bridge-settings-file) 'bridge-address))
(define user-device-name (hash-ref (file->value bridge-settings-file) 'user-device))
(define hue-user-name (hash-ref (file->value bridge-settings-file) 'hue-user-name))
(define device-type (hash-ref (file->value bridge-settings-file) 'device-type))
(define app-name (hash-ref (file->value bridge-settings-file) 'app-name))

(set! setup-needed
      (cond
        ((equal? bridge-address "0.0.0.0") #t)
        ((equal? user-device-name "") #t)
        (else #f)))

;; Default time for lights to transition while setting them. This does not effect cues.

(define default-set-time 1)

; Default error string from bridge.
; Eventually directly log errors instead of using this global variable.

(define bridge-error "")

;; Number of lights.

(define number-of-lights 16)

(define range-of-lights
  (+ number-of-lights 1))

;; Create lights.

(for ([i (in-range 1 range-of-lights)])
  (new light%
       [label (string-append "Hue-" (number->string i))]
       [parent primary-patch]
       [bulb i]
       [group 0]))

; Now it is time to create the main interaction window.

(define control-window (new shc-frame% [label "Simple Hue Control"]))

(define control-window-root-area
  (send control-window get-area-container))

; Next we create the panel to select the lights to control.

(define lights-select (new vertical-panel% [parent control-window-root-area]
                          [style '(border)]))

(define lights-select-title (new horizontal-panel% [parent lights-select]
                               [alignment '(center center)]))
(new message% [parent lights-select-title]
     [label "Select Lights to Cue"])

(define lights-select-panel-top (new horizontal-panel% [parent lights-select]
                                  [alignment '(center center)]
                                  [spacing 10]))
(define lights-select-panel-bottom (new horizontal-panel% [parent lights-select]
                                     [alignment '(center center)]
                                     [border 4]
                                     [spacing 3]))

(define lights-select-panel-buttons (new horizontal-panel% [parent lights-select]
                                      [alignment '(center center)]
                                      [border 4]
                                      [spacing 3]))

(define lights-select-panel-access-buttons (new horizontal-panel% [parent lights-select-panel-buttons]
                                            [alignment '(right center)]
                                            [border 4]
                                            [spacing 3]))

; Create some check boxes to select lights.
(let ([panel-divisor (ceiling (/ range-of-lights 2))])
  (for ([i (in-range 1 panel-divisor)])
    (new check-box% [parent lights-select-panel-top]
         [label (string-append "LX " (~a i))]
         [value #f]))
  (for ([i (in-range panel-divisor range-of-lights)])
    (new check-box% [parent lights-select-panel-bottom]
         [label (string-append "LX " (~a i))]
         [value #f])))

(define lights-clear-button (new button% [parent lights-select-panel-access-buttons]
                                 [label "Clear"]
                                 [callback (lambda (button event)
                                             (let ([number-of-boxes
                                                    (floor (/ range-of-lights 2))])
                                               (for ([i (in-range number-of-boxes)])
                                                 (send
                                                  (list-ref
                                                   (send lights-select-panel-top get-children)
                                                   i)
                                                  set-value #f))
                                               (for ([i (in-range number-of-boxes)])
                                                 (send
                                                  (list-ref
                                                   (send lights-select-panel-bottom get-children)
                                                   i)
                                                  set-value #f))))]))

(define lights-select-all-button (new button% [parent lights-select-panel-access-buttons]
                                      [label "Select All"]
                                      [callback (lambda (button event)
                                                  (let ([number-of-boxes
                                                         (floor (/ range-of-lights 2))])
                                                    (for ([i (in-range number-of-boxes)])
                                                      (send
                                                       (list-ref
                                                        (send
                                                         lights-select-panel-top
                                                         get-children)
                                                        i)
                                                       set-value #t))
                                                    (for ([i (in-range number-of-boxes)])
                                                      (send
                                                       (list-ref
                                                        (send
                                                         lights-select-panel-bottom
                                                         get-children)
                                                        i)
                                                       set-value #t))))]))

; Next is the panel for setting the attributes.
(define lights-attributes-panel (new vertical-panel% [parent control-window-root-area]
                              [style '(border)]))
(define lights-on-panel (new horizontal-panel% [parent lights-attributes-panel]))
(define lights-on-box (new radio-box% [parent lights-on-panel]
                          [label "On or Off?"]
                          [choices '("On" "Off")]
                          [style '(horizontal)]))

(define lights-bri-panel (new horizontal-panel% [parent lights-attributes-panel]))
(define lights-bri-box (new slider% [parent lights-bri-panel]
                             [label "Intensity?"]
                             [min-value 1]
                             [max-value 254]
                             [init-value 1]
                             [style '(horizontal)]
                             [min-width 10]
                             [stretchable-width 10]))

(define lights-color-panel (new horizontal-panel% [parent lights-attributes-panel]))
(define lights-color-box (new slider% [parent lights-color-panel]
                         [label "Hue?"]
                         [min-value 0]
                         [max-value 65535]
                         [init-value 0]
                         [style '(horizontal)]
                         [min-width 10]
                         [stretchable-width 10]))

(define lights-sat-panel (new horizontal-panel% [parent lights-attributes-panel]))
(define lights-sat-box (new slider% [parent lights-sat-panel]
                              [label "Saturation?"]
                              [min-value 0]
                              [max-value 254]
                              [init-value 0]
                              [style '(horizontal)]
                              [min-width 10]
                              [stretchable-width 10]))

; Now we need to send the cue to the bridge and save the cue.
; The Save Cue button saves the current lighting state. NOT the one about
; to be sent.

(define cue-set-save-panel (new horizontal-panel% [parent control-window-root-area]
                               [style '(border)]
                               [alignment '(center center)]))

(define cue-save-panel (new horizontal-panel% [parent cue-set-save-panel]
                          [alignment '(left center)]))

(define cue-save-button (new button% [parent cue-save-panel]
                           [label "Save"]
                           [min-height 50]
                           [callback (lambda (button event)
                                       (send save-cue-dialog show #t))]))
; Create A Dialog for Saving Cues.

(define save-cue-dialog (new dialog% [parent control-window]
                             [label "Save Cue"]))
(define save-cue-panel (new horizontal-panel% [parent save-cue-dialog]
                            [alignment '(left center)]
                            [min-width 200]))
(define save-cue-number-field (new text-field% [parent save-cue-panel]
                                   [label "Cue Number:"]))
(define save-cue-name-field (new text-field% [parent save-cue-panel]
                                 [label "Cue Name:"]))
(define save-cue-time-field (new text-field% [parent save-cue-panel]
                                 [label "Cue Time:"]))

(define save-cue-button-panel (new horizontal-panel% [parent save-cue-dialog]
                                   [alignment '(right center)]
                                   [min-width 200]))

(define save-cue-cancel (new button% [parent save-cue-button-panel]
                             [label "Cancel"]
                             [callback (lambda (button event)
                                         (send save-cue-name-field set-value "")
                                         (send save-cue-dialog show #f))]))

(define save-cue-ok (new button% [parent save-cue-button-panel]
                         [label "Save"]
                         [callback (lambda (button event)
                                     (let* ([new-cue-name (send save-cue-name-field get-value)]
                                            [new-cue-number
                                             (string->number (send save-cue-number-field get-value))]
                                            [new-cue-time
                                             (string->number (send save-cue-time-field get-value))])
                                       (new cue+c%
                                            [number new-cue-number]
                                            [label new-cue-name]
                                            [parent primary-cue-list]
                                            [json-value (retrieve-bridge-status
                                                         bridge-address
                                                         hue-user-name)]
                                            [time (* new-cue-time 10)])
                                       (send cue-list-display clear)
                                       (let ([cues (send primary-cue-list get-children)])
                                         (for ([i (in-range (length cues))])
                                           (send
                                            cue-list-display
                                            append
                                            (string-append
                                             (number->string (send (list-ref cues i) get-number))
                                             ". "
                                             (send (list-ref cues i) get-label)
                                             " - "
                                             (number->string
                                              (/ (send (list-ref cues i) get-time) 10))
                                             "s"))))
                                       (send save-cue-name-field set-value "")
                                       (send save-cue-number-field set-value
                                             (number->string (+ new-cue-number 1))))
                                     (resort-cue-choice cue-list-display)
                                     (resort-cue-list cue-list-display primary-cue-list)
                                     (save-show
                                      primary-patch
                                      primary-cue-list
                                      saved-show-write-port)
                                     (send save-cue-dialog show #f))]
                         [style '(border)]))

; Create Set Button

(define lights-set-panel (new horizontal-panel% [parent cue-set-save-panel]
                        [alignment '(right center)]))

(define lights-set-button (new button% [parent lights-set-panel]
                         [label "Set"]
                         [min-height 50]
                         [callback (lambda (button event)
                                     (get-attributes
                                      lights-select-panel-top
                                      lights-select-panel-bottom
                                      lights-on-box
                                      lights-bri-box
                                      lights-color-box
                                      lights-sat-box)
                                     (set-lights!
                                      (lightList (get-lights
                                                  lights-select-panel-top
                                                  lights-select-panel-bottom))
                                      primary-patch
                                      default-set-time
                                      bridge-address
                                      hue-user-name)
                                     (update-all-lights
                                      1 16
                                      first-status-row
                                      second-status-row
                                      bridge-address
                                      hue-user-name))]
                         [style '(border)]))

; Finally we need a window to show the status of all the lights.
; This Window gets its data from the bridge.

(define light-status-window (new shc-frame% [label "Channel Table"]
                       [min-width 1000]))

(create-status-boxes light-status-window number-of-lights)

(define first-status-row
  (list-ref (get-status-panels light-status-window) 0))

(define second-status-row
  (list-ref (get-status-panels light-status-window) 1))

; Create a Window for the Cue List.

(define cue-list-window (new shc-frame% [label "Main Cue List"]))

(define cue-list-window-root-area (send cue-list-window get-area-container))

(define cue-list-panel (new vertical-panel% [parent cue-list-window-root-area]
                          [alignment '(left center)]
                          [min-width 250]))

(define cue-list-display (new choice% [parent cue-list-panel]
                       [label "Cues:"]
                       [min-width 230]
                       [choices '()]))

(define restore-and-delete-panel (new horizontal-panel% [parent cue-list-panel]
                                   [alignment '(center center)]))

(define delete-panel (new horizontal-panel% [parent restore-and-delete-panel]
                         [alignment '(left center)]))

(define delete-button (new button% [parent delete-panel]
                           [label "Delete"]
                           [callback (lambda (button event)
                                       (delete-cue 
                                        primary-cue-list 
                                        (send cue-list-display get-selection))
                                       (send
                                        cue-list-display
                                        delete
                                        (send cue-list-display get-selection))
                                       (clear-show saved-show-file)
                                       (save-show
                                        primary-patch
                                        primary-cue-list
                                        saved-show-write-port))]))

(define restore-panel (new horizontal-panel% [parent restore-and-delete-panel]
                          [alignment '(right center)]))

(define restore-button (new button% [parent restore-panel]
                           [label "Restore"]
                           [callback (lambda (button event)
                                       (let ([thrd-1
                                              (thread (lambda ()
                                                        (restore-cue 
                                                         primary-cue-list 
                                                         (send cue-list-display get-selection) 
                                                         range-of-lights
                                                         primary-patch
                                                         bridge-address
                                                         hue-user-name)))]
                                             [thrd-2
                                              (thread (lambda () (update-all-lights
                                                                  1
                                                                  16
                                                                  first-status-row
                                                                  second-status-row
                                                                  bridge-address
                                                                  hue-user-name)))])
                                         #t))]
                                         
                           [style '(border)]))

;; "Show" Menu. Items actually in Bridge menu. Need to be moved to File menu in shc-gui.rkt.

(define hue-window-menu-show
  (list-ref (send (send control-window get-menu-bar) get-items) 2))

(define hue-window-menu-show-reload (new menu-item%
                                         [parent hue-window-menu-show]
                                         [label "Reload Previous Show"]
                                         [callback (lambda (menu event)
                                                     (prep-load-show saved-show-read-port)
                                                     (load-show
                                                      primary-patch
                                                      primary-cue-list
                                                      saved-show-read-port)
                                                     (let ([cues
                                                            (send primary-cue-list get-children)])
                                                       (append-cues cues cue-list-display))
                                                     (for ([i (in-range 1 range-of-lights)])
                                                       (send
                                                        (list-ref
                                                         (send assigned-light-panel get-children)
                                                         (- i 1))
                                                        set-value
                                                        (number->string
                                                         (send
                                                          (list-ref
                                                           (send primary-patch get-children)
                                                           (- i 1))
                                                          get-bulb))))
                                                     (send assigned-light-panel refresh))]))

;; Procedure to clear the saved show file.

;(define hue-window-menu-show-clear (new menu-item%
;                                        [parent hue-window-menu-show]
;                                        [label "Clear Previous Show"]
;                                        [callback (lambda (menu event)
;                                                    (clear-show saved-show-file))]))

;; Patch Dialog

(populate-patch range-of-lights primary-patch)

(create-patch-set-button
 primary-patch
 set-patch!
 primary-cue-list
 save-show
 saved-show-write-port)

;; Bridge Menu

(define hueWindowMenuBridge
  (list-ref (send (send control-window get-menu-bar) get-items) 2))

(define hueWindowMenuBridgeBridgeAddress (new menu-item% [parent hueWindowMenuBridge]
                                              [label "Set Bridge Address…"]
                                              [callback (lambda (menu event)
                                                          (send bridge-address-dialog show #t))]))
(define hueWindowMenuBridgeUserName (new menu-item% [parent hueWindowMenuBridge]
                                         [label "Set User Name…"]
                                         [callback (lambda (menu event)
                                                     (send userNameDialog show #t))]))

(define hueWindowMenuBridgeUpdateFirmware (new menu-item% [parent hueWindowMenuBridge]
                                               [label "Update Bridge Firmware"]
                                               [callback (lambda (menu event)
                                                           (send updateFirmwareDialog show #t))]))

;; Set Bridge Address Dialog

(define bridge-address-dialog (new dialog% [label "Enter Bridge Address"]
                                 [min-width 300]
                                 [min-height 100]))

(define bridge-address-panel (new horizontal-panel% [parent bridge-address-dialog]
                                [alignment '(left center)]
                                [min-width 300]))

(define bridge-address-field (new text-field% [parent bridge-address-panel]
                                [label "Bridge Address:"]
                                [init-value bridge-address]
                                [horiz-margin 20]))

(define set-bridge-address-panel (new horizontal-panel% [parent bridge-address-dialog]
                                   [alignment '(center center)]
                                   [min-width 300]))

(define cancel-bridge-address-button (new button% [parent set-bridge-address-panel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             ;(send bridge-address-field set-value "0.0.0.0")
                                             (send bridge-address-dialog show #f))]))

(define save-bridge-address-button (new button% [parent set-bridge-address-panel]
                               [label "Save"]
                               [callback (lambda (button event)
                                           (set! bridge-address (send bridge-address-field get-value))
                                           (let ([bridge-settings
                                                  (make-hash
                                                   (hash->list (file->value bridge-settings-file)))])
                                             (hash-set!
                                              bridge-settings
                                              'bridge-address
                                              (send bridge-address-field get-value))
                                             (with-output-to-file bridge-settings-file
                                               (lambda () (write bridge-settings))
                                               #:mode 'text
                                               #:exists 'replace)
                                             (send bridge-address-dialog show #f)))]
                               [style '(border)]))

; Set Bridge User Name Dialog

(define set-user-name!
  (lambda (device)
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   bridge-address "/api"
                   #:method 'POST
                   #:data
                   (jsexpr->string
                    (hash 'devicetype device))
                   #:headers
                   '("Content-Type: application/json")
                   #:content-decode '(json))])
      (let ([bridgeResponse (read-json jsonResponse)])
        (cond
          ((equal? (hash-keys (car bridgeResponse)) '(error))
           (set!
            bridge-error
            (string-append "Error: " (hash-ref (hash-ref (car bridgeResponse) 'error) 'description))))
          ((equal? (hash-keys (car bridgeResponse)) '(success))
           (set! hue-user-name (hash-ref (hash-ref (car bridgeResponse) 'success) 'username))
           (set! bridge-error "")))))))


(define userNameDialog (new dialog% [label "Enter Hue Bridge User Name"]
                            [min-width 550]
                            [min-height 100]))
(define userNameMessagePanel (new vertical-panel% [parent userNameDialog]
                                  [alignment '(center center)]
                                  [min-width 300]))
(define userNameMessage (new message% [parent userNameMessagePanel]
                             [label (string-join
                                     '("Enter Device Name (ie: My Macbook). "
                                     "Press Link Button on Bridge. Click \"Set\"."))]
                             [vert-margin 10]
                             [horiz-margin 20]
                             [auto-resize #t]))
(define userNamePanel (new vertical-panel% [parent userNameDialog]
                           [alignment '(left center)]
                           [min-width 200]
                           [stretchable-width 200]))
(define userDeviceNameField (new text-field% [parent userNamePanel]
                                 [label "Device Name:"]
                                 [init-value user-device-name]
                                 [horiz-margin 50]
                                 [min-width 200]
                                 [stretchable-width 200]))
(define setUserNamePanel (new horizontal-panel% [parent userNameDialog]
                              [alignment '(center center)]
                              [min-width 320]))
(define cancelUserName (new button% [parent setUserNamePanel]
                            [label "Cancel"]
                            [callback (lambda (button event)
                                        (send userNameDialog show #f))]))
(define saveUserName (new button% [parent setUserNamePanel]
                          [label "Set"]
                          [callback (lambda (button event)
                                      (let ([bridgeSettings
                                             (make-hash
                                              (hash->list
                                               (file->value bridge-settings-file)))])
                                        (set! user-device-name 
                                              (send userDeviceNameField get-value))
                                        (hash-set! bridgeSettings
                                                   'user-device
                                                   user-device-name)
                                        (set!
                                         device-type
                                         (string-append app-name "#" user-device-name))
                                        (hash-set! bridgeSettings
                                                   'device-type
                                                   device-type)
                                        (set-user-name! device-type)
                                        (cond
                                          ((equal? bridge-error "")
                                           (hash-set! bridgeSettings
                                                      'hue-user-name
                                                      hue-user-name)
                                           (with-output-to-file bridge-settings-file
                                             (lambda () (write bridgeSettings))
                                             #:mode 'text
                                             #:exists 'replace)
                                           (send userNameDialog show #f))
                                          (else
                                           (send userNameMessage set-label
                                                 (string-append 
                                                  bridge-error 
                                                  ". Enter Device Name (ie: My Macbook).
Press Link Button on Bridge. Click \"Set\"."))))))]
                          [style '(border)]))

; Bridge Update Dialog

(define portalError (string-join '("Error: Portal Connection Unavailable. "
                                   "Check the bridge's internet connection. "
                                   "Is the third light a steady blue?")))
(define updatingError (string-join '("Error: Bridge Is Currently Updating. "
                                     "Please Wait for Blue Lights to Return to Normal.")))

(define updateBridge
  (lambda ()
    (let-values ([(httpStatus httpHeader jsonResponse)
                  (http-sendrecv
                   bridge-address (string-append 
                                  (string-append "/api/" hue-user-name) 
                                  "/config/") 
                   #:method 'GET
                   #:headers
                   '("Content-Type: application/json")
                   #:content-decode '(json))])
      (let ([bridgeResponse (read-json jsonResponse)])
        (cond
          ((equal? (hash-ref (hash-ref bridgeResponse 'portalstate) 'signedon) #t)
           (cond
             ((equal?
               (hash-ref (hash-ref (hash-ref bridgeResponse 'swupdate) 'devicetypes) 'bridge)
               #t)
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
                                bridge-address (string-append 
                                               (string-append "/api/" hue-user-name) 
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
                        (set! bridge-error (string-append "Error: " 
                                                         (hash-ref 
                                                          (hash-ref (car bridgeResponse2) 'error) 
                                                          'description))))
                       ((equal? (hash-keys (car bridgeResponse2)) '(success))
                        (set! bridge-error "Error: No Update Available. Will Check."))))))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 1)
                 (set! bridge-error "Error: Update Still Downloading. Please Wait."))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 2)
                 ; Need to initiate Update.
                 (let-values ([(httpStatus2 httpHeader2 jsonResponse2)
                               (http-sendrecv
                                bridge-address (string-append 
                                               (string-append "/api/" hue-user-name) 
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
                        (set! bridge-error (string-append "Error: " 
                                                         (hash-ref 
                                                          (hash-ref (car bridgeResponse2) 'error) 
                                                          'description))))
                       ((equal? (hash-keys (car bridgeResponse2)) '(success))
                        (set! bridge-error "Done."))))))
                ((equal? (hash-ref (hash-ref bridgeResponse 'swupdate) 'updatestate) 3)
                 (set! bridge-error updatingError))))
             ((equal?
               (hash-ref (hash-ref (hash-ref bridgeResponse 'swupdate) 'devicetypes) 'bridge)
               #f)
              (set! bridge-error "No Update Available."))))
          (else (set! bridge-error portalError)))))))

(define updateFirmwareDialog (new dialog% [label "Update Bridge Firmware"]
                                  [min-width 350]
                                  [min-height 100]))

(define updateFirmwareMessagePanel (new vertical-panel% [parent updateFirmwareDialog]
                                        [alignment '(center center)]
                                        [min-width 200]))
(define updateFirmwareMessage (new message% [parent updateFirmwareMessagePanel]
                                   [label (string-join
                                           '("Update Firmware? Bridge Must "
                                             "Be Connected to the Internet."))]
                                   [horiz-margin 20]))

(define updateFirmwarePanel (new horizontal-panel% [parent updateFirmwareDialog]
                                 [alignment '(center center)]
                                 [min-width 290]))
(define closeUpdateFirmware (new button% [parent updateFirmwarePanel]
                                 [label "Close"]
                                 [callback (lambda (button event)
                                             (send updateFirmwareDialog show #f))]))
(define updateFirmware (new button% [parent updateFirmwarePanel]
                            [label "Update"]
                            [callback (lambda (button event)
                                        (updateBridge)
                                        (send updateFirmwareMessage set-label bridge-error))]))

; Show the Windows

(send light-status-window show #t)
(send cue-list-window show #t)
(send control-window show #t)

;; If "Bridge Settings.shc" is newly created.

(when (equal? setup-needed #t)
  (define setup-dialog (new dialog% [parent control-window]
                            [label "Setup"]))
  (define setup-panel (new vertical-panel% [parent setup-dialog]
                           [alignment '(center top)]))
  (new message% [parent setup-panel]
       [label "Please set Bridge IP address and User Name.
Menus to do so located under the Bridge Menu."]
       [horiz-margin 7]
       [vert-margin 10])
  (new button% [parent setup-panel]
       [label "Ok"]
       [callback
        (lambda  (button event)
          (send setup-dialog show #f))]
       [style '(border)])
  (send setup-dialog show #t))
