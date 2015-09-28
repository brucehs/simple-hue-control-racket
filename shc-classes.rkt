#lang racket

(provide cue-list%
         cue%
         patch%
         light%)

; Cue List and Cue Classes
; Perhaps Scenes could be used for Cueing instead. They can have the
; transitiontime value attached. However, the state of the scene is not
; available via an API call. It may be better to ask the user to specify
; a time upon saving a cue.

(define cue-list%
  (class object%
    (super-new)
    (init-field label)
    (init-field [children '()])
    (define/public (get-label) label)
    (define/public (get-children) children)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-children listOfChildren)
      (set-field! children this listOfChildren))))

(define cue%
  (class object%
    (super-new)
    (init-field number)
    (init-field label)
    (init-field parent)
    (init-field [json-value (hash 'light (hash
                                          'on #t
                                          'bri 0
                                          'hue 0
                                          'sat 0
                                          'xy (list 0 0)
                                          'ct 0
                                          'alert "none"
                                          'effect "none"
                                          'colormode "hs"
                                          'reachable #t)
                                  'type "Extended color light"
                                  'name "Generic Name"
                                  'modelid "LCT001"
                                  'swversion "")])
    ;; Time is in milliseconds.
    ;; Does not need to be adjusted when sent to bridge.
    (init-field [time 4])
    (define/public (get-number) number)
    (define/public (get-label) label)
    (define/public (get-parent) parent)
    (define/public (get-json) json-value)
    (define/public (get-time) time)
    (define/public (set-number new-number)
      (set-field! number this new-number))
    (define/public (set-label new-label)
      (set-field! label this new-label))
    (define/public (set-json new-json)
      (set-field! json-value this new-json))
    (define/public (set-time new-time)
      (cond
        ((equal? (string->number new-time) #f)
         (set-field! time this 0))
        (else
         (set-field! time this
                     (inexact->exact (* (string->number new-time) 10))))))
    (begin
      (let ([child-lst (get-field children parent)])
        (cond
          ((member? number (cue-numbers child-lst))
           (let-values ([(pre-lst post-lst) (split-at child-lst (- number 1))])
             (for/list ([cue post-lst])
               (send cue set-number (+ (send cue get-number) 1)))
             (set-field! children parent
                         (append pre-lst (list this) post-lst))))
          (else (set-field! children parent
                            (append (get-field children parent)
                                    (list this)))))))))

;; Procedures for determining whether a cue needs to be inserted.
(define cue-numbers
  (lambda (cues)
    (cond
      ((empty? cues) '())
      (else
       (cons (send (car cues) get-number)
             (cue-numbers (cdr cues)))))))

(define member?
  (lambda (item lst)
    (cond
      ((empty? lst) #f)
      ((equal? item (car lst)) #t)
      (else (member? item (cdr lst))))))

; A Lights Class to allow for grouping and level comparison for restoring cues.
; Also a Patch Class to act as a parent for all lights

(define patch%
  (class object%
    (super-new)
    (init-field [label ""])
    (init-field [children '()])
    (define/public (get-label) label)
    (define/public (get-children) children)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-children listOfChildren)
      (set-field! children this listOfChildren))))

; Patch Object for Testing
(define mainPatch (new patch% [label "Main Patch"]))

(define light%
  (class object%
    (super-new)
    (init-field parent)
    (init-field [label ""])
    (init-field [bulb 1])
    (init-field [group 0])
    (init-field [state (hash
                        'on #f
                        'bri 1
                        'hue 0
                        'sat 0)])
    (define/public (get-label) label)
    (define/public (get-parent) parent)
    (define/public (get-bulb) bulb)
    (define/public (get-group) group)
    (define/public (get-state) state)
    (define/public (set-label new-label)
      (set-field! label this new-label))
    (define/public (set-bulb new-bulb)
      (set-field! bulb this new-bulb))
    (define/public (set-group new-group)
      (cond
        ((isGroup? new-group)
         (set-field! group this new-group))))
    (define/public (set-state newState)
      (cond
        ((hash? newState)
         (set-field! state this newState))))
    (begin (set-field! children parent
                       (append (get-field children parent)
                               (list this))))))

; Procedure for determining if an assigned group number is valid.

(define isGroup?
  (lambda (group)
    (cond
      ((and
        (and
         (exact-nonnegative-integer? group)
         (>= group 0))
        (<= group 16))
       #t)
      (else #f))))