#lang racket

(define cueList%
  (class object%
    (super-new)
    (init-field [label ""])
    (init-field [children '()])
    (define/public (get-label) label)
    (define/public (get-children) children)))

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
    (define/public (get-label) label)
    (define/public (get-parent) parent)
    (define/public (get-json) jsonResponse)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-json newJson)
      (set-field! jsonResponse this newJson))
    (define/public (set-parent parentList)
      (set-field! children parentList 
                  (append (get-field children parentList) (list this)))
      (set-field! parent this parentList))))

(define newList (new cueList% [label "Test List"]))
(define newCueOne (new cue% [label "Test Cue 1"]))
(define newCueTwo (new cue% [label "Test Cue 2"]))

;Example of getting state from json

;(hash-ref (hash-ref 
;            (send (list-ref (send newList get-children) 0) get-json) 
;             'light) 
;            'on)