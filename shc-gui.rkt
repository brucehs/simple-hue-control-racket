#lang racket/gui

(require framework
         "shc-classes.rkt"
         "shc-save_load.rkt"
         "shc-settings.rkt"
         "shc-show_control.rkt")

(provide shc-frame%)

(provide populate-patch
         create-patch-set-button)

(provide lamp-patch-dialog
         assigned-light-panel)

(define saved-show-write-port
  (open-output-file saved-show-file #:mode 'text #:exists 'can-update))

; Create new shc-frame% class using framework. Add Patch & Reset Patch
; menu-items to Edit Menu.

(define ext-frame%
    (frame:standard-menus-mixin
    (frame:status-line-mixin frame:basic%)))

(define shc-frame%
  (class ext-frame%
    (super-new)
    (define/override (edit-menu:create-clear?) #f)
    (define/override (edit-menu:between-select-all-and-find edit-menu)
      (new separator-menu-item% [parent edit-menu])
      (new menu-item%
           [parent edit-menu]
           [label "Patch"]
           [callback (lambda (menu event)
                       (send lamp-patch-dialog show #t))])
      (new menu-item%
           [parent edit-menu]
           [label "Reset Patch 1-to-1"]
           [callback (lambda (menu event)
                       (set-patch-to-default!
                        primary-patch
                        assigned-light-panel)
                       (save-show
                        primary-patch
                        primary-cue-list
                        saved-show-write-port))])
    (begin
      (new menu%
           [parent (send this get-menu-bar)]
           [label "Show"])
      (new menu%
           [parent (send this get-menu-bar)]
           [label "Lamp"])
      (new menu%
           [parent (send this get-menu-bar)]
           [label "Bridge"])
      (frame:reorder-menus this)))))

; Create the Patch Dialog

(define lamp-patch-dialog (new dialog% [label "Patch Lamps"]
                               [min-width 300]
                               [min-height 600]))

(define assigned-light-panel (new vertical-panel% [parent lamp-patch-dialog]
                                  [alignment '(left top)]
                                  [horiz-margin 10]
                                  [vert-margin 15]
                                  [min-width 50]
                                  [min-height 600]))

(define patch-button-panel (new horizontal-panel% [parent lamp-patch-dialog]
                                [alignment '(center bottom)]
                                [horiz-margin 10]
                                [vert-margin 15]))

(define patch-cancel-button (new button% [parent patch-button-panel]
                                 [label "Cancel"]
                                 [callback (lambda (button event)
                                             (send lamp-patch-dialog show #f))]
                                 [horiz-margin 15]))

; Functions to create the content of the Patch Dialog that requires
; Simple Hue Control Main.rkt objects.

(define populate-patch
  (lambda (range patch-obj)
    (for ([i (in-range 1 range)])
      (new text-field%
           [parent assigned-light-panel]
           [label (cond
                    ((< i 10)
                     (string-append "  Channel " (number->string i) "         ""Bulb:"))
                    (else
                     (string-append "  Channel " (number->string i) "        ""Bulb:")))]
           [init-value (number->string
                        (send
                         (list-ref (send patch-obj get-children) (- i 1))
                         get-bulb))]
           [min-width 40]
           [stretchable-width 40]
           [vert-margin 5]))))

(define create-patch-set-button
  (lambda (patch-obj set-method! cue-list-obj save-proc write-port)
    (new button% [parent patch-button-panel]
                              [label "Set"]
                              [callback (lambda (button event)
                                          (set-method!
                                           patch-obj
                                           assigned-light-panel)
                                          (save-proc
                                           patch-obj
                                           cue-list-obj
                                           write-port)
                                          (send lamp-patch-dialog show #f))]
                              [style '(border)]
                              [horiz-margin 15])))

;; Comment out. For testing only.

;(define test-frame (new shc-frame%
;                        [label "Testing"]
;                        [width 300]
;                        [height 200]))
;
;(send test-frame show #t)
;
;(define test-menu-bar (send test-frame get-menu-bar))
;
;(define patch-item (list-ref (send (list-ref (send test-menu-bar get-items) 1) get-items) 9))
