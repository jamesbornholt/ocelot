#lang rosette

(require (prefix-in $ racket))
(provide (except-out (all-defined-out) universe)
         (rename-out [make-universe universe]))


;; universe --------------------------------------------------------------------

(struct universe (atoms inverse) #:transparent)
(define (make-universe atoms)
  (let ([inverse (for/hash ([(a i) (in-indexed atoms)]) (values a i))])
    (universe
     atoms
     (lambda (t) (hash-ref inverse t)))))
(define (universe-size universe)
  (length (universe-atoms universe)))
