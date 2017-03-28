#lang rosette

(require "../ocelot.rkt" 
         (only-in "../lang/ast.rkt" relation-arity)
         (only-in "../engine/matrix.rkt" matrix-entries matrix? tuple->idx)
         rackunit (prefix-in $ racket))

(provide (all-defined-out))

(define-syntax-rule (check-matrix-equal U R contents)
  (let ([expected-indices (list->set (map (curry tuple->idx U) contents))]
        [actual-contents (matrix-entries R)])
    ; check everything in it should be in it
    (for ([(v i) (in-indexed actual-contents)])
      (when v
        (check-true (set-member? expected-indices i)
          (format "should not contain ~a" i))))
    ; check everything that should be in it is in it
    (for ([i expected-indices])
      (check-true
        ($and ($< i (length actual-contents)) (list-ref actual-contents i))
        (format "should contain ~a" i)))))

(define-syntax (test-expr stx)
  (syntax-case stx ()
    [(_ U E [(r1 v1) ...] result)
     (let ([loc (syntax->location stx)])
       (quasisyntax/loc stx
         (with-check-info* (list (make-check-location '#,loc))
           (thunk
            (define lbnds (list (make-exact-bound r1 v1) ...))
            (define bnds (bounds U lbnds))
            (unless (null? result)
              (check-equal? (length (first result)) (relation-arity E)))
            (define E* (interpret E bnds))
            (check-true (matrix? E*))
            (check-matrix-equal U E* result)))))]))

(define-syntax (test-formula-solve stx)
  (syntax-case stx ()
    [(_ U E [(r1 v1) ...] expected)
     (let ([loc (syntax->location stx)])
       (quasisyntax/loc stx
         (with-check-info* (list (make-check-location '#,loc))
           (thunk
            (let ([Ex E])
              (define lbnds (list (make-exact-bound r1 v1) ...))
              (define bnds (bounds U lbnds))
              (define E* (interpret Ex bnds))
              (define S (with-handlers ([exn:fail? (const (unsat))])
                          (solve (assert E*))))
              (check-equal? (sat? S) expected))))))]))

(define-syntax (test-formula stx)
  (syntax-case stx ()
    [(_ U F [(r1 v1) ...] result)
     (let ([loc (syntax->location stx)])
       (quasisyntax/loc stx
         (with-check-info* (list (make-check-location '#,loc))
           (thunk
            (define lbnds (list (make-exact-bound r1 v1) ...))
            (define bnds (bounds U lbnds))
            (define F* (interpret F bnds))
            (check-equal? F* result)))))]))

(define-syntax (test-matrix stx)
  (syntax-case stx ()
    [(_ U E [(r1 v1) ...] pred)
     (let ([loc (syntax->location stx)])
       (quasisyntax/loc stx
         (with-check-info* (list (make-check-location '#,loc))
           (thunk
            (define lbnds (list (make-exact-bound r1 v1) ...))
            (define bnds (bounds U lbnds))
            (define E* (interpret E bnds))
            (pred E*)))))]))

(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

; clear state but without restarting the solver
(define (clear-most-state!)
  (current-oracle (oracle))
  (clear-asserts!)
  (clear-terms!)
  (solver-clear (current-solver)))
