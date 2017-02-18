#lang rosette

(require "../ocelot.rkt" "../engine/symmetry.rkt"
         (only-in "../lang/ast.rkt" relation-name relation-arity node/formula?))
(provide define-sig scope alloy-fact alloy-run alloy-check
         clear-alloy-facts! clear-alloy!)


;; Define signatures using (define-sig name (type ...))
;; e.g. (define-sig reads-from (list Read Write))
;;      defines a relation of arity 2 that maps elements of Read to
;;      elements of Write (which should be already defined)
(define sigs (make-parameter '()))
(struct sig (relation type) #:transparent)
(define-syntax (define-sig stx)
  (syntax-case stx ()
    [(_ name [subname : subtype ...] ...)
     (syntax/loc stx
       (begin
         (define name (let ([r (declare-relation 1 (symbol->string 'name))])
                        (sigs (cons (sig r (list r)) (sigs)))
                        r))
         (define subname
           (let* ([type (cons name (list subtype ...))]
                  [r (declare-relation (length type) (symbol->string 'subname))])
             (sigs (cons (sig r type) (sigs)))
             r)) ...))]))


;; Create bounds for all relations defined by define-sig.
(struct alloy-scope (bounds domain) #:transparent)
(define (scope n)
  ; Find leaf signatures (those whose type includes themselves)
  (define leaves
    (for/list ([s (sigs)] #:when (member (sig-relation s) (sig-type s)))
      (let ([r (sig-relation s)])
        (unless (= (relation-arity r) 1)
          (error 'scope "self-referencing relation of arity > 1: ~v" r))
        r)))
  ; Instantiate leaf signatures
  (define relation->atoms
    (for/hash ([r leaves])
      (values r (for/list ([i n])
                  (string->symbol (format "~a$~a" (relation-name r) i))))))
  (define atoms (append* (hash-values relation->atoms)))
  ; Create bounds for all signatures
  (define bnds
    (for/list ([s (sigs)])
      (apply make-product-bound 
             (sig-relation s)
             (for/list ([r (sig-type s)]) (hash-ref relation->atoms r)))))
  ; Enforce domain restrictions: the domain of each relation must be
  ; contained in its given type.
  (define (fold-op t [op ->])
    (for/fold ([ret (car t)]) ([r (cdr t)]) (op ret r)))
  (define domain
    (for/list ([s (sigs)] #:unless (hash-has-key? relation->atoms (sig-relation s)))
      (define arity (length (sig-type s)))
      (define left (if (= arity 1)
                       (sig-relation s)
                       (fold-op (cons (sig-relation s) (make-list (sub1 arity) univ)) join)))
      (in left (car (sig-type s)))))
  ; Enforce the multiplicity of each signature:
  ; sig A { f: B } means that all a: A | one a.f
  (define mults
    (for/list ([s (sigs)] #:when (> (length (sig-type s)) 1))
      (all ([x (car (sig-type s))])
        (and (one (join x (sig-relation s)))
             (in (join x (sig-relation s)) (fold-op (cdr (sig-type s)) ->))))))
  (alloy-scope (bounds (universe atoms) bnds) (append domain mults)))


;; Assert a formula
(define alloy-facts (make-parameter '()))
(define (alloy-fact f)
  (unless (node/formula? f)
    (raise-argument-error 'alloy-assert "formula?" f))
  (alloy-facts (cons f (alloy-facts))))


;; Run a formula
(define (alloy-run f scope)
  (define bnds (alloy-scope-bounds scope))
  (define interp (instantiate-bounds bnds))
  (define pre (for/list ([a (alloy-facts)]) (interpret* a interp)))
  (define dom (for/list ([d (alloy-scope-domain scope)]) (interpret* d interp)))
  (define post (interpret* f interp))
  (define sbp (generate-sbp interp (alloy-scope-bounds scope)))
  (sat? (solve (assert (and sbp (apply && pre) (apply && dom) post)))))


;; Check a formula
(define (alloy-check f scope)
  (define bnds (alloy-scope-bounds scope))
  (define interp (instantiate-bounds bnds))
  (define pre (for/list ([a (alloy-facts)]) (interpret* a interp)))
  (define dom (for/list ([d (alloy-scope-domain scope)]) (interpret* d interp)))
  (define post (interpret* f interp))
  (define sbp (generate-sbp interp (alloy-scope-bounds scope)))
  (unsat? (solve (assert (and sbp (apply && dom) (apply && pre) (not post))))))


;; Clear all Alloy asserts
(define (clear-alloy-facts!)
  (alloy-facts '()))


;; Clear all Alloy state
(define (clear-alloy!)
  (clear-alloy-facts!)
  (sigs '()))
