#lang rosette

(require "solver.rkt" "cmsat.rkt"
         (prefix-in @ rosette)
         rackunit)
(provide make-cnf-solver (all-from-out "solver.rkt"))

(struct cnf-solver (solver var->const cache)
  #:methods gen:solver
  [(define/generic assert-cnf assert-clause)
   (define/generic solve-cnf solve)
   (define/generic get-value-cnf get-value)
   (define (assert-clause self cl)
     (match-define (cnf-solver solver var->const cache) self)
     (define (fresh [var (gensym)]) ; get a fresh 1-indexed CNF var
       (hash-set! var->const var (add1 (hash-count var->const)))
       (hash-ref var->const var))
     (define new-clauses '())
     (define (rec F)
       (hash-ref!
        cache
        F
        (thunk
         (match F
           [(expression (== @!) f1)
            (define a (fresh))
            (define a_f1 (rec f1))
            (set! new-clauses (append (list (list a a_f1) (list (- a) (- a_f1))) new-clauses))
            a]
           [(expression (== <=>) a b)
            (rec (&& (|| (! a) b) (|| (! b) a)))]
           [(expression op fns ...)
            (define a (fresh))
            (define a_fns (map rec fns))
            (define-values (a->a_fns a_fns->a)
              (match op
                [(== @&&) (values (for/list ([x a_fns]) (list (- a) x))
                                  (list (cons a (map - a_fns))))]
                [(== @||) (values (list (cons (- a) a_fns))
                                  (for/list ([x a_fns]) (list a (- x))))]
                [_ (error 'to-cnf "unknown op ~v" op)]))
            (set! new-clauses (append a->a_fns a_fns->a new-clauses))
            a]
           [(constant x ty)
            (unless (equal? ty boolean?)
              (error 'to-cnf "non-boolean variable ~v (type ~v)" x ty))
            (fresh F)]
           [#t  ; hack: just assert an unused fresh variable
            (define a (fresh))
            (set! new-clauses (cons (list a) new-clauses))
            a]
           [#f  ; hack: assert a & !a
            (define a (fresh))
            (set! new-clauses (append (list (list a) (list (- a))) new-clauses))
            a]
           [_ (error 'to-cnf "unknown formula ~v" F)]))))
     (define a_F (rec cl))
     (for ([cnf new-clauses])
       (assert-cnf solver cnf))
     (assert-cnf solver (list a_F)))
   (define (solve self)
     (match-define (cnf-solver solver var->const _) self)
     (define ret (solve-cnf solver))
     (if ret
         (sat (for/hash ([(v i) var->const] #:when (constant? v))
                (values v (get-value-cnf solver i))))
         (unsat)))])

(define (make-cnf-solver [make-sat-solver make-cmsat-solver])
  (cnf-solver (make-sat-solver) (make-hash) (make-hash)))

(module+ test
  (define s (make-cnf-solver))
  (define-symbolic* a b c boolean?)
  (assert-clause s (|| a b))
  (check-true (sat? (solve s)))
  (assert-clause s (! a))
  (check-true (sat? (solve s)))
  (assert-clause s (! b))
  (check-false (sat? (solve s)))
  (define s1 (make-cnf-solver))
  (assert-clause s1 #t)
  (check-true (sat? (solve s1)))
  (assert-clause s1 #f)
  (check-false (sat? (solve s1))))