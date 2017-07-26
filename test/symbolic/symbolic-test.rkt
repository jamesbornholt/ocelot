#lang rosette

(require ocelot "../util.rkt"
         rosette/lib/angelic rosette/lib/synthax
         rackunit rackunit/text-ui)

(define U (universe '(a b c d)))
(define A1 (declare-relation 1 "A1"))
(define B1 (declare-relation 1 "B1"))
(define C1 (declare-relation 1 "C1"))
(define A2 (declare-relation 2 "A2"))
(define A3 (declare-relation 3 "A3"))

(define (grammar depth ops terminals)
  (define (rec bnd)
    (if (<= bnd 0)
        (apply choose* terminals)
        (let ()
          (define e0 (rec (sub1 bnd)))
          (define e1 (rec (sub1 bnd)))
          (define-symbolic* xi boolean? [(length ops)])
          (let loop ([xi xi][ops ops])
            (cond [(null? xi) (apply choose* terminals)]
                  [(car xi)   ((car ops) e0 e1)]
                  [else       (loop (cdr xi) (cdr ops))])))))
  (rec depth))

(define (test-+)
  (define E1 (thunk (grammar 2 (list +) (list A1 B1))))
  
  (test-formula-solve U (= (E1) C1) [(A1 '((a) (b))) (B1 '((c) (d))) (C1 '((a) (b) (c) (d)))] #t)
  (test-formula-solve U (= (E1) C1) [(A1 '((a) (b))) (B1 '((c) (d))) (C1 '((a) (b) (c)))] #f)
  )

(define (test-combo)
  (define E1 (thunk (grammar 2 (list + ->) (list A1 B1))))

  (test-formula-solve U (= (E1) C1) [(A1 '((a) (b))) (B1 '((c) (d))) (C1 '((a) (b) (c) (d)))] #t)
  (test-formula-solve U (= (E1) C1) [(A1 '((a) (b))) (B1 '((c) (d))) (C1 '((a) (b) (c)))] #f)

  (test-formula-solve U (= (E1) A2) [(A1 '((a) (b))) (B1 '((c) (d))) (A2 '((a c) (a d) (b c) (b d)))] #t)
  (test-formula-solve U (= (E1) A2) [(A1 '((a) (b))) (B1 '((c) (d))) (A2 '((a c) (a d) (b c)))] #f)
  (test-formula-solve U (= (E1) A2) [(A1 '((a) (b))) (B1 '((c) (d))) (A2 '((a a) (a b) (b a) (b b)))] #t)

  (test-formula-solve U (= (E1) A3) [(A1 '((a) (b))) (B1 '((c) (d)))
                                     (A3 '((a c a) (a c b) (a d a) (a d b) (b c a) (b c b) (b d a) (b d b)))] #t)
  )

(define (test-caching)
  (define-symbolic p q boolean?)

  (define X (if p A1 B1))
  (define Y (if (&& p q) (+ X none) (- X none)))

  ; p = #t => Y = A1
  ; p = #f => Y = B1
  ; but if X is cached incorrectly in the left branch of Y, and then reused in
  ; the right branch of Y, then Y = A1 regardless of p
  (test-formula-solve U (= Y A1) [(A1 '((a))) (B1 '((b)))] #t)
  (test-formula-solve U (= Y B1) [(A1 '((a))) (B1 '((b)))] #t)
  )

(define (test-sketch-under-comprehension)
  (define E1 (set ([a A1]) (choose (in a B1) (in a C1))))

  (test-formula-solve U (= E1 A1) [(A1 '((a) (b))) (B1 '()) (C1 '((a) (b)))] #t)
  (test-formula-solve U (= E1 A1) [(A1 '((a) (b))) (B1 '()) (C1 '((a)))] #f)
  (test-formula-solve U (= E1 A1) [(A1 '((a) (b))) (B1 '((a) (b))) (C1 '((a) (b)))] #t)
  (test-formula-solve U (= E1 A1) [(A1 '((a))) (B1 '((a) (b))) (C1 '((a)))] #t)
  )

(define (test-eval)
  ; Ocelot depends on equality being preserved across evaluate; if this doesn't
  ; hold, then evaluation can be messed up
  (define-symbolic p boolean?)

  (define F (if p A1 B1))
  (define model (solve (assert p)))
  (check-true (equal? (evaluate F model) A1))
  )


(define solve-tests
  (test-suite
   "solve tests"
   #:before (thunk (printf "----- solve tests -----\n"))
   (test-+)
   (test-combo)
   (test-caching)
   (test-sketch-under-comprehension)
   (test-eval)))

(module+ test
  (time (run-tests solve-tests)))
