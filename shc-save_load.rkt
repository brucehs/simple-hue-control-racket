#lang racket

(provide patch->hash
         light->hash
         cue-list->hash
         cue->hash)



(provide patch?
         light?
         cue-list?
         cue?)

;; Procedures to save object fields to hashes.

(define patch->hash
  (lambda (patch)
    (hash 'object 'patch
          'label (send patch get-label))))

(define light->hash
  (lambda (patch light)
    (hash 'object 'light
          'label (send (list-ref (send patch get-children) light) get-label)
          'bulb (send (list-ref (send patch get-children) light) get-bulb)
          'group (send (list-ref (send patch get-children) light) get-group)
          'state (send (list-ref (send patch get-children) light) get-state))))

(define cue-list->hash
  (lambda (cue-list)
    (hash 'object 'cue-list
          'label (send cue-list get-label))))

(define cue->hash
  (lambda (cue-list cue)
    (hash 'object 'cue
          'label (send (list-ref (send cue-list get-children) cue) get-label)
          'json (send (list-ref (send cue-list get-children) cue) get-json)
          'time (send (list-ref (send cue-list get-children) cue) get-time))))

;; Procedure to save and load object fields to/from show file.

;; TUDU: Need to extend such that they iteravely save and pull data from file.

(define save-hash
  (lambda (hsh port)
    (writeln hsh port)))

(define load-hash
  (lambda (port)
  (read port)))

;; Procedures to determine type of restored object.

(define patch?
  (lambda (hsh)
    (if (equal? (hash-ref hsh 'object) 'patch) #t #f)))

(define light?
  (lambda (hsh)
    (if (equal? (hash-ref hsh 'object) 'light) #t #f)))

(define cue-list?
  (lambda (hsh)
    (if (equal? (hash-ref hsh 'object) 'cue-list) #t #f)))

(define cue?
  (lambda (hsh)
    (if (equal? (hash-ref hsh 'object) 'cue) #t #f)))