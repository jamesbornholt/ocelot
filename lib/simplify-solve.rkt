#lang rosette

(require (prefix-in ast/ "../lang/ast.rkt")
         "../engine/engine.rkt"
         "../lang/bounds.rkt"
         "../engine/interpretation.rkt"
         "../lang/universe.rkt"
         "../lang/sketch.rkt"
         rosette/solver/smt/z3)
(provide simplify/solve ast-cost)

(current-bitwidth #f)


(define (get-ctor s)
  (let-values ([(struct-type skipped?) (struct-info s)])
    (struct-type-make-constructor struct-type)))


(define (walk-expr e proc accum)
  (for/all ([e e])
    (let ([accum* (proc e accum)])
  (match e
    [(ast/node/expr/op _ children)
     (for/fold ([a accum*]) ([c children]) (walk-expr c proc a))]
    [(ast/node/expr/comprehension _ decls formula)
     (define a* (for/fold ([a accum*]) ([c (map cdr decls)]) (walk-expr c proc a)))
     (walk-expr formula proc a*)]
    [(ast/node/formula/op children)
     (for/fold ([a accum*]) ([c children]) (walk-expr c proc a))]
    [(ast/node/formula/quantified _ decls formula)
     (define a* (for/fold ([a accum*]) ([c (map cdr decls)]) (walk-expr c proc a)))
     (walk-expr formula proc a*)]
    [(ast/node/formula/multiplicity _ expr)
     (walk-expr expr proc accum*)]
    [_ accum*]))))


(define (ast-cost e)
  (walk-expr e (lambda (e accum) (+ accum 1)) 0))


; This procedure finds the smallest expression (defined by number of AST nodes)
; that is equivalent to the given expr under the given constraints (e.g., typo
; information). Only works on simple expressions (e.g. no set comprehensions).
(define (simplify/solve expr constraints [usize 3])
  ; first, find all the relations mentioned in the expr
  (define relations
    (walk-expr expr
               (lambda (e accum)
                 (if (ast/node/expr/relation? e)
                     (set-add accum e)
                     accum))
               (set)))
  ; also find relations mentioned in the constraints, which will need to be
  ; bound (though will not be available in the synthesized expression)
  (define relations*
    (walk-expr constraints
               (lambda (e accum)
                 (if (ast/node/expr/relation? e)
                     (set-add accum e)
                     accum))
               (set)))

  ; shortcut: use any joins in the expression as terminals in the sketch
  (define joins
    (walk-expr expr
               (lambda (e accum)
                 (if (ast/node/expr/op/join? e)
                     (set-add accum e)
                     accum))
               (set)))

  ; instantiate all those relations
  (define atoms (build-list usize values))
  (define U (universe atoms))
  (define bnds
    (bounds U (for/list ([r (set-union relations relations*)])
                (apply make-product-bound r (make-list (ast/relation-arity r) atoms)))))
  (define interp (instantiate-bounds bnds))

  ; evaluate the constraints
  (define constraints* (interpret* constraints interp #:cache? #t))

  ; iterate
  (let outer ([depth 0])
    (define arity (ast/relation-arity expr))
    (define sketch (expression-sketch depth arity
                                      (list ast/+ ast/- ast/& ast/->)
                                      (append (set->list relations)
                                              (set->list joins)
                                              (list ast/none ast/univ ast/iden))
                                      #:balanced? #f))
    (define cost (ast-cost sketch))

    (define equiv (ast/= expr sketch))
    (define equiv* (interpret* equiv interp #:cache? #t))
    (define xs (symbolics interp))
    (define solver (z3))
    (solver-assert solver (list (forall xs (=> constraints* equiv*))))
    (let inner ([s (unsat)])
      (define sol (solver-check solver))
      (cond [(sat? sol)
             (define c* (evaluate cost sol))
             (solver-assert solver (list (< cost c*)))
             (inner sol)]
            [(unsat? s)
             (solver-shutdown solver)
             (if (> depth 6)
                 expr
                 (outer (add1 depth)))]
            [else (solver-shutdown solver)
                  (evaluate sketch s)]))))
