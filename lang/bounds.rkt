#lang racket

(require racket/generator (only-in "ast.rkt" relation-arity))
(provide (all-defined-out))


; A bound is a relation and two lists of tuples `lower` and `upper`.
(struct bound (relation lower upper) #:transparent)
; A bounds object is a universe and a collection of bound? instances.
(struct bounds (universe entries) #:transparent)

; Error-checking constructors for bounds
(define (make-bound relation lower upper)
  (for ([t lower])
    (unless (and (list? t) (= (length t) (relation-arity relation)))
      (raise-arguments-error 'make-bound "bounds must contain tuples" "lower" t)))
  (for ([t upper])
    (unless (and (list? t) (= (length t) (relation-arity relation)))
      (raise-arguments-error 'make-bound "bounds must contain tuples" "upper" t)))
  (bound relation lower upper))

(define (make-exact relation contents)
  (make-bound relation contents contents))

(define (make-upper relation contents)
  (make-bound relation '() contents))

; Get the upper bound for a relation r in a bounds? object
(define (get-upper-bound bnds r)
  (for/first ([b (bounds-entries bnds)] #:when (equal? (bound-relation b) r))
    (bound-upper b)))

; get a list of all relations bound by a bounds? object
(define (bounds-variables bnds)
  (for/list ([b (bounds-entries bnds)]) (bound-relation b)))

; Combine several sets of bounds, which must be mutually disjoint and share the
; same universe
(define (bounds-union . lbnds)
  (define U (bounds-universe (car lbnds)))
  (bounds U (for*/list ([bnds lbnds][bnd (bounds-entries bnds)]) bnd)))
