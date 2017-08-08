#lang rosette

(require (prefix-in ast/ "ast.rkt") (prefix-in $ racket) rosette/lib/angelic)
(provide expression-sketch)


; Construct an expression sketch from a grammar.
; The arguments specify the maximum depth of the grammar unrolling,
; the non-terminal operators, and the terminal relations in the grammar.
; Optionally, a maximum arity can be chosen for expressions in the grammar,
; and the num-joins parameter controls how many nested joins are allowed.
(define (expression-sketch depth arity ops terminals [max-arity 3] [num-joins 1]
                           #:balanced? [balanced? #t])
  ; ops must contain some way of producing identity expressions so that the
  ; grammar includes all exprs *up to* the given depth.
  (unless (or (member ast/& ops) (member ast/+ ops))
    (set! ops (cons ast/& ops)))

  ; produce an expression of the given arity and depth
  (define (rec arity bnd [num-joins num-joins])
    (define exprs
      (if (= bnd 0)
          ; terminals if depth 0
          (filter (λ (e) (equal? (ast/relation-arity e) arity)) terminals)
          ; non-terminals if depth > 0
          (begin
            (define-values (terms cache)
              (for*/fold ([terms '()][cache (hash)])
                         ([op (in-list ops)] #:unless (and (equal? op ast/join) (= num-joins 0))
                          [args (in-list (possible-args op arity max-arity))])
                (let ([num-joins-in-subexprs (- num-joins (if (equal? op ast/join) 1 0))])
                  (define subexprs
                    (let ([local-cache cache])  ; track which exprs have been used in this instantiation
                      (for/list ([a (in-list args)])
                        (let* ([key (cons num-joins-in-subexprs a)]
                               [exprs (hash-ref local-cache key '())])
                          (cond [(> (length exprs) 0)  ; an expression already exists
                                 (set! local-cache (hash-set local-cache key (cdr exprs)))
                                 (car exprs)]
                                [else  ; construct an expression and cache it
                                 (let ([expr (rec a (- bnd 1) num-joins-in-subexprs)])
                                   (set! cache (hash-set cache key (append (hash-ref cache key '()) (list expr))))
                                   expr)])))))
                  (values (append terms (if (for/or ([e (in-list subexprs)]) ($false? e)) '() (list (apply op subexprs))))
                          cache))))
            (append terms
                    (if balanced?
                        '()
                        (filter (λ (e) (equal? (ast/relation-arity e) arity)) terminals))))))
    ; if there are no subexprs at this level, return #f
    ; (checked by all-subexprs-exist?)
    (if (null? exprs)
        #f
        (apply choose* exprs)))

  (rec arity depth num-joins))


; given an operation and an arity, produce a list of arities for
; arguments that could produce that arity when op is applied.
; max-arity is the highest arity that will be used in joins.
; for example:
;  (possible-args ~ 2) = '((2))
;  (possible-args ~ 1) = '()
;  (possible-args + 2) = '((2 2))
;  (possible-args -> 3) = '((1 2) (2 1))
;  (possible-args join 2) = '((1 3) (2 2) (3 1))
; this need not be precise, since constructors will throw exceptions
; when called with incorrect arities
(define (possible-args op arity [max-arity 3])
  (match op
    [(== ast/->)
     (for/list ([arity1 (in-range 1 arity)])
       (list arity1 (- arity arity1)))]
    [(== ast/join)
     (for/list ([arity1 (in-range 1 (+ arity 2))]
                [arity2 (in-range (+ arity 1) 0 -1)]
                #:when (and (<= arity1 max-arity) (<= arity2 max-arity)))
       (list arity1 arity2))]
    [(== ast/<:)
     (list (list 1 arity))]
    [(== ast/:>)
     (list (list arity 1))]
    [(or (== ast/^) (== ast/*) (== ast/~))
     (if (= arity 2) '((2)) '())]
    [(? ast/unary-op?)
     (list (list arity))]
    [(ast/prefab inputs _)
     (inputs arity)]
    [else
     (list (list arity arity))]))

; returns two values:
;  a list of (op, arg) pairs to instantiate
;  a hash from arity to # of instances to instantiate
(define (possible-subexprs ops arity [max-arity 3])
  (for*/fold ([nodes '()][subtrees (hash)])
             ([op (in-list ops)][args (in-list (possible-args op arity))])
    (values (append nodes (list (cons op args)))
            (for/fold ([subtrees subtrees])
                      ([a args])
              (hash-set subtrees a (max (hash-ref subtrees a 0)
                                        (for/sum ([b (in-list args)]) (if (= a b) 1 0))))))))
