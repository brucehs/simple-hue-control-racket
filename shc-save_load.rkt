#lang racket

(provide patch->hash
         light->hash
         cue-list->hash
         cue->hash)

(provide save-show
         prep-load-show
         load-show)

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
          'id light
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
          'id cue
          'label (send (list-ref (send cue-list get-children) cue) get-label)
          'json (send (list-ref (send cue-list get-children) cue) get-json)
          'time (send (list-ref (send cue-list get-children) cue) get-time))))

;; Procedure to save and load object fields to/from show file.

(define save-show
  (lambda (patch cue-list port)
    (file-position port 0)
    (for ([i (in-range (length (send patch get-children)))])
      (writeln (light->hash patch i) port))
    (for ([i (in-range (length (send cue-list get-children)))])
      (writeln (cue->hash cue-list i) port))))

;; Procedure to load light object data back into show.

(define reload-light
  (lambda (patch id hsh)
    (letrec ([label (hash-ref hsh 'label)]
             [bulb (hash-ref hsh 'bulb)]
             [group (hash-ref hsh 'group)]
             [state (hash-ref hsh 'state)])
      (send (list-ref (send patch get-children) id) set-label label)
      (send (list-ref (send patch get-children) id) set-bulb bulb)
      (send (list-ref (send patch get-children) id) set-group group)
      (send (list-ref (send patch get-children) id) set-state state))))

;; Resets the saved show file to the beginning for reading purposes.

(define prep-load-show
  (lambda (port)
    (file-position port 0)))

;; Main Procedure to reload the entire show data.

;; TUDU: Add restoring cues.

(define load-show
  (lambda (patch cue-list port)
    (let ([object-hash (read port)])
      (cond
        ((equal? object-hash eof) '(done))
        ((equal? (hash-ref object-hash 'object) 'light)
         (reload-light
          patch
          (hash-ref object-hash 'id)
          object-hash)
         (cons
           (list
            'light
            (hash-ref object-hash 'id))
          (load-show patch cue-list port))) 
        ((equal? (hash-ref object-hash 'object) 'cue)
         (cons
          (list
           'cue
           (hash-ref object-hash 'id)); For testing purposes now. Need to write procedure to restore state to cue object.
               (load-show patch cue-list port)))))))

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