#lang racket

(require "../lang/universe.rkt" "matrix.rkt" (prefix-in $ racket)
         (only-in rosette and or not for/all for*/all)
         (only-in rosette/base/core/bool && || => <=> !))

(provide (all-defined-out))

(define (matrix/nary-op universe op args)
  (for/fold ([A (car args)]) ([B (in-list (cdr args))])
    (op universe A B)))

(define (matrix/and universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (matrix (for/list ([a (in-list A)][b (in-list B)]) (and a b)))))

(define (matrix/or universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (matrix (for/list ([a (in-list A)][b (in-list B)]) (or a b)))))

(define (matrix/difference universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (matrix (for/list ([a (in-list A)][b (in-list B)]) (and a (not b))))))

(define (matrix/dot universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (let* ([univSize (universe-size universe)]
           [arityA (matrix-arity universe A)]
           [arityB (matrix-arity universe B)]
           [arity ($+ arityA arityB -2)]
           [size ($expt univSize arity)]
           [sizeB ($expt univSize arityB)]
           [c ($quotient sizeB univSize)]
           [vB (list->vector B)])
      (define res ($make-vector size #f))
      (for ([(iVal i) (in-indexed A)] #:unless ($false? iVal))
        (let* ([rowHead ($* ($remainder i univSize) c)]
               [rowTail ($+ rowHead c)]
               [base ($quotient i univSize)])
          (for ([j (in-range rowHead rowTail)])
            (let ([b (vector-ref vB j)])
              (unless ($false? b)
                (let ([retVal (&& iVal b)])
                  (unless ($false? retVal)
                    (let ([k ($+ ($* base c) ($remainder j c))])
                      ($vector-set! res k (|| ($vector-ref res k) retVal))))))))))
      (matrix ($vector->list res)))))

(define (matrix/cross universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (matrix (for*/list ([a (in-list A)][b (in-list B)]) (and a b)))))

(define (matrix/transpose universe A)
  (for/all ([A (matrix-entries A)])
    (let* ([univSize (universe-size universe)])
      (matrix (for*/list ([i (in-range univSize)] [j (in-range univSize)])
                (list-ref A (+ (* univSize j) i)))))))

(define (matrix/closure universe A)
  (let ([univSize (universe-size universe)])
    (for/all ([A (matrix-entries A)])
      (let loop ([Y A] [i 1])
        (let ([rY (matrix Y)])
          (cond [(>= i univSize) rY]
                [else (let ([Y.Y   (matrix/dot universe rY rY)])
                        (if (for/and ([y (matrix-entries Y.Y)]) ($false? y))
                            rY
                            (let ([Y∪Y.Y (matrix/or universe rY Y.Y)])
                              (loop (matrix-entries Y∪Y.Y) (* 2 i)))))]))))))

; evaluate D <: A
(define (matrix/domain universe D A)
  (let ([univSize (universe-size universe)])
    (for*/all ([A (matrix-entries A)][Ds (matrix-entries D)])
      (let* ([arityA (matrix-arity universe A)]
             [denomA ($expt univSize ($- arityA 1))]
             [size ($expt univSize arityA)])
        (matrix (for/list ([i (in-range size)][a (in-list A)])
                  (and a (list-ref Ds ($quotient i denomA)))))))))

; evaluate A :> R
(define (matrix/range universe A R)
  (let ([univSize (universe-size universe)])
    (for*/all ([A (matrix-entries A)][Rs (matrix-entries R)])
      (let* ([arityA (matrix-arity universe A)]
             [size ($expt univSize arityA)])
        (matrix (for/list ([i (in-range size)][a (in-list A)])
                  (and a (list-ref Rs ($remainder i univSize)))))))))

; is A a subset of B? i.e., if x in A, then x in B ≡ x in A ⇒ x in B
(define (matrix/subset? universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (apply && (for/list ([a (in-list A)][b (in-list B)]) (=> a b)))))

; is A equal to B? i.e. x in A iff x in B
(define (matrix/equal? universe A B)
  (for*/all ([A (matrix-entries A)][B (matrix-entries B)])
    (apply && (for/list ([a (in-list A)][b (in-list B)]) (<=> a b)))))

; does A contain exactly one element?
(define (matrix/one? universe A)
  (for/all ([A (matrix-entries A)])
    (let loop ([disj #f] [pred #t] [A A])
      (cond
        [(null? A) (and pred disj)]
        [else (let* ([a (car A)]
                     [me (=> a (not disj))])
                (loop (or disj a) (and pred me) (cdr A)))]))))

; does A contain any element?
(define (matrix/some? universe A)
  (for/all ([A (matrix-entries A)])
    (apply || A)))

; does A contain a given tuple?
(define (matrix/contains? universe tuple A)
  (for/all ([A (matrix-entries A)])
    (let ([arity (matrix-arity universe A)])
      (unless (= arity (length tuple))
        (raise-argument-error 'matrix/contains? "tuple of correct length" tuple))
      (let ([idx (tuple->idx universe tuple)])
        (list-ref A idx)))))

