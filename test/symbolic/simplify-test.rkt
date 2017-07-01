#lang rosette

(require racket/hash rackunit
         "../util.rkt"
         ocelot
         (prefix-in ast/ ocelot/lang/ast))

(file-stream-buffer-mode (current-output-port) 'none)

(define terminals
  (list (ast/declare-relation 1 "A")
        (ast/declare-relation 1 "B")
        (ast/declare-relation 2 "C")
        (ast/declare-relation 2 "D")
        ast/none
        ast/univ
        ast/iden))

(define nonterminals
  (list ast/+ ast/- ast/& ast/-> ast/join ast/<: ast/:>))

; Interpret the relations once with respect to the universe so we get the
; variables for their contents immediately
(define U (universe '(1 2 3 4)))
(define (make-top relation)
  (define arity (ast/relation-arity relation))
  (define contents
    (apply cartesian-product (make-list arity (universe-atoms U))))
  (make-upper-bound relation contents))
(define bnds
  (bounds U (for/list ([r terminals] #:unless (ast/node/expr/constant? r))
              (make-top r))))
(define interp (instantiate-bounds bnds))

; Cache for smaller expressions to feed into larger ones
(define cache terminals)

; Run tests up to a given depth
(define (test depth)
  (define n 0)
  (for ([d depth])
    (define new-cache cache)
    (for ([nt nonterminals])
      (for* ([a cache][b cache])
        (set! n (add1 n))
        (when (= (modulo n 1000) 0)
          (printf "~v\n" n))
        (with-handlers ([exn:fail?
                         (lambda (e) (unless (string-contains? (exn-message e) "arity")
                                       (raise e)))])
          (define F (nt a b))
          (set! new-cache (cons F new-cache))
          (define F* (interpret* F interp))
          (define Fsimp (simplify F))
          (define Fsimp* (interpret* Fsimp interp))
          (define S (parameterize ([term-cache (make-hash)])
                      (verify (assert (equal? F* Fsimp*)))))
          (check-pred unsat? S (~a (ast->datum F)))
          (clear-most-state!))))
    (set! cache new-cache))
  (printf "tested ~v of ~v expressions\n" (- (length cache) (length terminals)) n))
  
  
; For tests
(module+ test
  (test 1))

; For full tests
(module+ main
  (test 2))
