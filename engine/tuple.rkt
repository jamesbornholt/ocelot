#lang racket

(require racket/generic)
(provide (except-out (all-defined-out) tupleset)
         (rename-out [make-tupleset tupleset]))

(define (make-tupleset arity indices)
  (tupleset arity indices (sort (set->list indices) <)))
(struct tupleset (arity index-set index-list)
  #:transparent
  #:methods gen:set
  [(define/generic member? set-member?)
   (define/generic intersect set-intersect)
   (define/generic count set-count)
   (define/generic add set-add)
   (define/generic subtract set-subtract)
   (define (set-member? self x)
     (member? (tupleset-index-set self) x))
   (define (set-intersect st0 . args)
     (unless (for/and ([a args]) (= (tupleset-arity st0) (tupleset-arity a)))
       (raise-arguments-error 'set-intersect "tuplesets with same arity"))
     (make-tupleset (tupleset-arity st0)
                    (apply intersect (map tupleset-index-set (cons st0 args)))))
   (define (set-count st)
     (count (tupleset-index-set st)))
   (define (set-add st v)
     (make-tupleset (tupleset-arity st) (add (tupleset-index-set st) v)))
   (define (set-subtract st0 . args)
     (unless (for/and ([a args]) (= (tupleset-arity st0) (tupleset-arity a)))
       (raise-arguments-error 'set-subtract "tuplesets with same arity"))
     (make-tupleset (tupleset-arity st0)
                    (apply subtract (map tupleset-index-set (cons st0 args)))))]
  #:property prop:sequence
  (lambda (self) (in-list (tupleset-index-list self))))
(define (tupleset-min ts)
  (first (tupleset-index-list ts)))
