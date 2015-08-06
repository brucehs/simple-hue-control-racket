#lang racket

(provide cueList%
         cue%
         patch%
         light%)

; Cue List and Cue Classes
; Perhaps Scenes could be used for Cueing instead. They can have the
; transitiontime value attached. However, the state of the scene is not
; available via an API call. It may be better to ask the user to specify
; a time upon saving a cue.

(define cueList%
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

(define cue%
  (class object%
    (super-new)
    (define stateJson     (hash
                           'on #t
                           'bri 0
                           'hue 0
                           'sat 0
                           'xy (list 0 0)
                           'ct 0
                           'alert "none"
                           'effect "none"
                           'colormode "hs"
                           'reachable #t))
    (init-field [label ""])
    (init-field [jsonResponse (hash 'light stateJson
                                    'type "Extended color light"
                                    'name "Generic Name"
                                    'modelid "LCT001"
                                    'swversion "")])
    (init-field [parent '(object:cueList%)])
    ; Time is in milliseconds. Does not need to be adjusted when sent to bridge.
    (init-field [time 4])
    (define/public (get-label) label)
    (define/public (get-parent) parent)
    (define/public (get-json) jsonResponse)
    (define/public (get-time) time)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-json newJson)
      (set-field! jsonResponse this newJson))
    (define/public (set-time newTime)
      (cond
        ((equal? (string->number newTime) #f)
         (set-field! time this 0))
        (else
         (set-field! time this
                     (inexact->exact (* (string->number newTime) 10))))))
    (begin (set-field! children parent
                                    (append (get-field children parent)
                                            (list this))))))

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
    (init-field [label ""])
    (init-field [parent '()])
    (init-field [group 0])
    (init-field [state (hash
                        'on #f
                        'bri 1
                        'hue 0
                        'sat 0)])
    (define/public (get-label) label)
    (define/public (get-parent) parent)
    (define/public (get-group) group)
    (define/public (get-state) state)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-group newGroup)
      (cond
        ((isGroup? newGroup)
         (set-field! group this newGroup))))
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
