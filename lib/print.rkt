#lang racket

(require "../lang/ast.rkt")

(provide ast->datum ast->alloy)

;; converting ASTs to s-exprs --------------------------------------------------
(define (op-name node)
  (if (node/formula/op/||? node)
      'or
      (string->symbol (regexp-replace #rx"node/(expr|formula)/op/" (symbol->string (object-name node)) ""))))
(define (ast->datum ast)
  (match ast
    [(node/expr/relation arity name) (string->symbol name)]
    [(node/expr/constant expr type) type]
    [(node/expr/comprehension arity decls formula)
     `(set (,@(for/list ([d (in-list decls)]) `[,(ast->datum (car d)) ,(ast->datum (cdr d))])) ,(ast->datum formula))]
    [(node/formula/quantified quantifier decls formula)
     `(,quantifier (,@(for/list ([d (in-list decls)]) `[,(ast->datum (car d)) ,(ast->datum (cdr d))])) ,(ast->datum formula))]
    [(node/formula/multiplicity mult expr)
     `(,mult ,(ast->datum expr))]
    [(node/expr/op arity children)
     `(,(op-name ast) ,@(for/list ([c (in-list children)]) (ast->datum c)))]
    [(node/formula/op children)
     `(,(op-name ast) ,@(for/list ([c (in-list children)]) (ast->datum c)))]))

;; converting ASTs to alloy expressions ----------------------------------------
(define (op->alloy op)
  (match op
    [(? node/expr/op/join?) "."]
    [_ (symbol->string (op-name op))]))
(define (ast->alloy ast)
  (match ast
    [(node/expr/relation arity name) name]
    [(node/expr/constant expr type) (symbol->string type)]
    [(node/expr/comprehension arity decls formula)
     (let* ([fmt-decl (lambda (d) (format "~a: ~a" (ast->alloy (car d)) (ast->alloy (cdr d))))]
            [decl-string (string-join (map fmt-decl decls) ", ")]
            [pred (ast->alloy formula)])
       (format "{ ~a | ~a }" decl-string pred))]
    [(node/formula/quantified quantifier decls formula)
     (let* ([fmt-decl (lambda (d) (format "~a: ~a" (ast->alloy (car d)) (ast->alloy (cdr d))))]
            [decl-string (string-join (map fmt-decl decls) ", ")]
            [pred (ast->alloy formula)])
       (format "(~a ~a | ( ~a ))" quantifier decl-string pred))]
    [(node/formula/multiplicity mult expr)
     (format "~a ~a" mult (ast->alloy expr))]
    [(or (node/expr/op _ children) (node/formula/op children))
     (format "(~a)" (if (unary-op? ast)
                        (format "~a ~a" (op->alloy ast) (ast->alloy (first children)))
                        (string-join (map ast->alloy children) (format " ~a " (op->alloy ast)))))]
    #;[(node/formula/op children)
     (format "(~a)" (string-join (map ast->alloy children) (format " ~a " (op->alloy ast))))]))
