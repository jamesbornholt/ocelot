#lang racket

(require racket/require
         (filtered-in (lambda (n) (and (regexp-match? #rx"^node/" n) n)) "../lang/ast.rkt")
         (prefix-in ast/ "../lang/ast.rkt"))

(provide simplify)


;; utility stuff ---------------------------------------------------------------

(define ast-cache (make-hash))
(define (ast<? a b)
  (< (hash-ref! ast-cache a (thunk (hash-count ast-cache)))
     (hash-ref! ast-cache b (thunk (hash-count ast-cache)))))

(define (get-ctor s)
  (let-values ([(struct-type skipped?) (struct-info s)])
    (struct-type-make-constructor struct-type)))


;; canonicalize commutative operations to simplify equality checking -----------

(define (commutative? t)
  (or (node/expr/op/+? t) (node/expr/op/&? t)))

(define (canonicalize-commutative-ops t)
  (let ([ctor (get-ctor t)])
    (match t
      [(node/expr/op arity args)
       (let ([args (map canonicalize-commutative-ops args)])
         (ctor arity (if (commutative? t)
                         (sort args ast<?)
                         args)))]
      [x x])))


;; simplify expressions --------------------------------------------------------

(define (iden n x)
  (for/fold ([x* x]) ([i (sub1 n)])
    (ast/-> x* x)))

(define (empty? t)
  (match t
    [(node/expr/op/+ arity args)
     (for/and ([a (in-list args)]) (empty? a))]
    [(node/expr/op/- arity args)
     (empty? (car args))]
    [(or (node/expr/op/& arity args)
         (node/expr/op/-> arity args)
         (node/expr/op/join arity args)
         (node/expr/op/<: arity args)
         (node/expr/op/:> arity args))
     (for/or ([a (in-list args)]) (empty? a))]
    [(== ast/none) #t]
    [x #f]))
(define (full? t)
  (match t
    [(node/expr/op/+ arity args)
     (for/or ([a (in-list args)]) (full? a))]
    [(node/expr/op/- arity args)
     (for/and ([a (in-list (cdr args))]) (empty? a))]
    [(or (node/expr/op/& arity args)
         (node/expr/op/-> arity args)
         (node/expr/op/join arity args)
         (node/expr/op/<: arity args)
         (node/expr/op/:> arity args))
     (for/and ([a (in-list args)]) (full? a))]
    [(== ast/univ) #t]
    [x #f]))
(define (subset? a b)
  (cond [(empty? a) #t]
        [(full? b) #t]
        [else (match* (a b)
                [(a (node/expr/op/+ arity args))
                 (if (member a args) #t #f)]
                [((node/expr/op/& arity args) b)
                 (if (member b args) #t #f)]
                [((node/expr/op/- arity args) b)
                 (equal? (car args) b)]
                [(_ _) #f])]))

(define (simplify-identity-values t)
  (let ([ctor (get-ctor t)])
    (match t
      [(node/expr/op/+ arity args)
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list-no-order (? empty?) a) a]
           [(list _ ... (? full?) _ ...) (iden arity ast/univ)]
           [(list a a) a]
           [(list a b) (cond [(subset? a b) b]
                             [(subset? b a) a]
                             [else (ctor arity args)])]
           [_ (ctor arity args)]))]
      [(node/expr/op/& arity args)
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list _ ... (? empty?) _ ...) (iden arity ast/none)]
           [(list-no-order (? full?) a) a]
           [(list a a) a]
           [(list a b) (cond [(subset? a b) a]
                             [(subset? b a) b]
                             [else (ctor arity args)])]
           [_ (ctor arity args)]))]
      [(node/expr/op/- arity args)
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list a (? empty?)) a]
           [(list a a) (iden arity ast/none)]
           [(list a b) (cond [(subset? a b) (iden arity ast/none)]
                             [else (ctor arity args)])]
           [_ (ctor arity args)]))]
      [(node/expr/op/-> arity args)
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list _ ... (? empty?) _ ...) (iden arity ast/none)]
           [_ (ctor arity args)]))]
      [(node/expr/op/join arity args)
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list-no-order a (? empty?)) (iden arity ast/none)]
           [_ (ctor arity args)]))]
      [(node/expr/op/<: arity args)  ; (<: s r) = tuples of r that start with x ∈ s
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list (? full?) a) a]
           [(list (? empty?) a) (iden arity ast/none)]
           [(list a (? empty?)) (iden arity ast/none)]
           [(list a a) a]
           [_ (ctor arity args)]))]
      [(node/expr/op/:> arity args)  ; (:> r s) = tuples of r that end with x ∈ s
       (let ([args (map simplify-identity-values args)])
         (match args
           [(list a (? full?)) a]
           [(list a (? empty?)) (iden arity ast/none)]
           [(list (? empty?) a) (iden arity ast/none)]
           [(list a a) a]
           [_ (ctor arity args)]))]
      [(node/expr/op arity args)
       (ctor arity (map simplify-identity-values args))]
      [x x])))


;; simplify to fixed point -----------------------------------------------------

(define (simplify t)
  (let loop ([n 100][t t])
    (let ([t* (simplify-identity-values
               (canonicalize-commutative-ops t))])
      (cond [(= n 0) t*]
            [(equal? t t*) t*]
            [else (loop (sub1 n) t*)]))))
