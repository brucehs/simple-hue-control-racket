#lang racket

(define cueLists%
  (class object%
    (super-new)
    (init-field [label ""])
    (define/public (get-label) label)))

(define cues%
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
    (define/public (get-label) label)
    (define/public (get-json) jsonResponse)
    (define/public (set-label newLabel)
      (set-field! label this newLabel))
    (define/public (set-json newJson)
      (set-field! jsonResponse this newJson))))