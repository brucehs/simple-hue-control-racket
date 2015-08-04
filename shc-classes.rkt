#lang racket

; Cue List and Cue Classes
; Perhaps Scenes could be used for Cueing instead. They can have the
; transitiontime value attached. However, the state of the scene is not
; available via an API call. It may be better to ask the user to specify
; a time upon saving a cue.

(provide cueList% cue%)

(define cueList%
  (class object%
    (super-new)
    (init-field [label ""])
    (init-field [children '()])
    (define/public (get-label) label)
    (define/public (get-children) children)
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
    (define/public (set-parent parentCueList)
      (set-field! children parentCueList 
                  (append (get-field children parentCueList) (list this)))
      (set-field! parent this parentCueList))
    (define/public (set-time newTime)
      (cond
        ((equal? (string->number newTime) #f)
         (set-field! time this 0))
        (else
         (set-field! time this
                     (inexact->exact (* (string->number newTime) 10))))))))