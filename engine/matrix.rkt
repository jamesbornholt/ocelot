#lang rosette

(require (prefix-in $ racket) "../lang/universe.rkt")
(provide (all-defined-out))

(struct matrix (entries) #:transparent)

(define (matrix-arity universe mat)
  (define len (length (if (matrix? mat) (matrix-entries mat) mat)))
  (exact-round ($/ (log len) (log (universe-size universe)))))

(define (tuple->idx universe tuple)
  (let ([base (length (universe-atoms universe))])
    (let loop ([idx 0] [mult 1] [tuple (reverse tuple)])
      (cond [(empty? tuple) idx]
            [else (loop (+ idx (* ((universe-inverse universe) (car tuple)) mult))
                        (* mult base)
                        (cdr tuple))]))))
(define (idx->tuple universe arity idx)
  (define usize (universe-size universe))
  (for/list ([i (in-range arity)])
    (list-ref (universe-atoms universe)
              (remainder (quotient idx (expt usize (- arity i 1))) usize))))

(define (singleton-matrix universe idx)
  (matrix (for/list ([i (in-range (universe-size universe))]) (= i idx))))
