#lang racket

(let ([light-objects (lightList lightsToCue)])
  (for ([i (in-range (length light-objects))])
    (send (list-ref
           (send mainPatch get-children)
           (- (list-ref light-objects i) 1))
          set-state (get-light-state
                     (list-ref light-objects i)
                     bridgeAddress
                     hueUserName))))
