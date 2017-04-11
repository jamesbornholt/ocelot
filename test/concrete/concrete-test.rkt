#lang racket

(require ocelot "../util.rkt"
         (only-in "../../engine/matrix-ops.rkt" matrix/contains?)
         rackunit rackunit/text-ui)

(define U (universe '(a b c d)))
(define A1 (declare-relation 1 "A1"))
(define B1 (declare-relation 1 "B1"))
(define C1 (declare-relation 1 "C1"))
(define A2 (declare-relation 2 "A2"))
(define B2 (declare-relation 2 "B2"))
(define C2 (declare-relation 2 "C2"))
(define A3 (declare-relation 3 "A3"))
(define B3 (declare-relation 3 "B3"))
(define C3 (declare-relation 3 "C3"))

;; expressions -----------------------------------------------------------------

(define (test-+)
  (define E1 (+ A1 B1))
  (test-expr U E1 [(A1 '((a) (b))) (B1 '((c)))] '((a) (b) (c)))

  (define E2 (+ A2 B2))
  (test-expr U E2 [(A2 '((a a) (b b))) (B2 '((b b) (c d)))] '((a a) (b b) (c d)))

  (define E3 (+ A3 B3))
  (test-expr U E3 [(A3 '((a a a) (b b b))) (B3 '())] '((a a a) (b b b)))
  (test-expr U E3 [(A3 '()) (B3 '((c c c)))] '((c c c)))

  (define E4 (+ A2 B2 C2))
  (test-expr U E4 [(A2 '((a a) (b b))) (B2 '((a c))) (C2 '((c c) (d d)))] 
    '((a a) (b b) (a c) (c c) (d d)))

  (check-exn exn:fail? 
    (thunk 
      (define E5 (+ A1 B2))
      (test-expr U E5 [(A1 '((a))) (B2 '((a b)))] '())))
  )

(define (test-&)
  (define E1 (& A1 B1))
  (test-expr U E1 [(A1 '((a) (b))) (B1 '((b)))] '((b)))

  (define E2 (& A2 B2))
  (test-expr U E2 [(A2 '((a b) (a c) (b c))) (B2 '((a c) (c a)))] '((a c)))
  (test-expr U E2 [(A2 '((a a) (b b))) (B2 '((c c)))] '())

  (define E3 (& A3 B3))
  (test-expr U E3 [(A3 '((a a a) (b b b))) (B3 '((a a a) (c c c)))] '((a a a)))

  (define E4 (& A2 B2 C2))
  (test-expr U E4 [(A2 '((a a) (b b))) (B2 '((a a) (b b) (c c))) (C2 '((a a) (c c)))] '((a a)))
  
  (check-exn exn:fail? 
    (thunk 
      (define E5 (& A1 B2))
      (test-expr U E5 [(A1 '((a))) (B2 '((a b)))] '())))
  )

(define (test--)
  (define E1 (- A1 B1))
  (test-expr U E1 [(A1 '((a) (b))) (B1 '((b)))] '((a)))

  (define E2 (- A2 B2))
  (test-expr U E2 [(A2 '((a b) (a c) (b c))) (B2 '((a c) (c a)))] '((a b) (b c)))
  (test-expr U E2 [(A2 '((a a))) (B2 '((a a) (b b)))] '())

  (define E3 (- A3 B3))
  (test-expr U E3 [(A3 '((a a a) (b b b))) (B3 '((a a a) (c c c)))] '((b b b)))
  
  (define E4 (- A2 B2 C2))
  (test-expr U E4 [(A2 '((a a) (b b) (c c))) (B2 '((d d) (b b))) (C2 '((a a) (d a)))] '((c c)))

  (check-exn exn:fail? 
    (thunk
      (define E5 (- A1 B2))
      (test-expr U E5 [(A1 '((a))) (B2 '((a b)))] '())))
  )
 
(define (test-join)
  (define E1 (join A2 B2))
  (test-expr U E1 [(A2 '((a b) (a c) (b c))) (B2 '((a c) (c a)))] '((a a) (b a)))

  (define E2 (join A3 B1))
  (test-expr U E2 [(A3 '((a b c) (b b c))) (B1 '((c)))] '((a b) (b b)))

  (define E3 (join A3 B3))
  (test-expr U E3 [(A3 '((a a a) (b c d) (c d a) (c d d) (d a a) (d d d)))
                   (B3 '((b b b) (b c d) (c c c) (d b a) (d c d)))]
                  '((b c b a) (b c c d) (c d b a) (c d c d) (d d b a) (d d c d)))

  (define E4 (join A2 B1))
  (test-expr U E4 [(A2 '((a a))) (B1 '((b)))] '())
  (test-expr U E4 [(A2 '((a a))) (B1 '())] '())

  (define E5 (join A2 B2 C2))
  (test-expr U E5 [(A2 '((a b) (a c))) (B2 '((b d) (c a))) (C2 '((d a) (a b)))] '((a a) (a b)))
  (test-expr U E5 [(A2 '((a a))) (B2 '()) (C2 '((a a)))] '())

  (check-exn exn:fail? 
    (thunk
      (define E6 (join A1 B1))
      (test-expr U E6 [(A1 '((a))) (B1 '((b)))] '())))
  (check-exn exn:fail? 
    (thunk
      (define E7 (join A2 B1 C1))
      (test-expr U E7 [(A2 '((a a))) (B1 '((a))) (C1 '((b)))] '())))
  )

(define (test-~)
  (define E1 (~ A2))
  (test-expr U E1 [(A2 '((a b) (c d)))] '((b a) (d c)))
  (test-expr U E1 [(A2 '((a a) (c c)))] '((a a) (c c)))

  (check-exn exn:fail?
    (thunk
     (define E2 (~ A1))
     (test-expr U E2 [(A1 '())] '()))))

(define (test-->)
  (define E1 (-> A1 B1))
  (test-expr U E1 [(A1 '((a) (b))) (B1 '((c)))] '((a c) (b c)))
  (test-expr U E1 [(A1 '((a) (b))) (B1 '((c) (d)))] '((a c) (a d) (b c) (b d)))
  (test-expr U E1 [(A1 '((a) (b))) (B1 '())] '())
  (test-expr U E1 [(A1 '()) (B1 '((a) (b)))] '())

  (define E2 (-> A2 B1))
  (test-expr U E2 [(A2 '((a a) (b b))) (B1 '((c) (d)))] '((a a c) (a a d) (b b c) (b b d)))

  (define E3 (-> A1 B1 C1))
  (test-expr U E3 [(A1 '((a))) (B1 '((b) (c))) (C1 '((d)))] '((a b d) (a c d)))
  )

(define (test-none)
  (test-expr U none [] '()))

(define (test-univ)
  (test-expr U univ [] '((a) (b) (c) (d))))

(define (test-iden)
  (test-expr U iden [] '((a a) (b b) (c c) (d d))))

(define (test-set)
  (define E1 (set ([x A1]) (in x B1)))
  (test-expr U E1 [(A1 '((a) (b) (c))) (B1 '((a) (b)))] '((a) (b)))
  (test-expr U E1 [(A1 '((c))) (B1 '((a) (b)))] '())

  (define E2 (set ([x A1][y B1]) (and (in x C1) (in y C1))))
  (test-expr U E2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a) (b)))] '((a b) (b b)))
  (test-expr U E2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a) (b) (c)))]
                  '((a b) (a c) (b b) (b c)))
  (test-expr U E2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((c) (d)))] '())

  (check-exn exn:fail?
    (thunk
     (define E3 (set ([x A2]) (in x A2)))
     (test-expr U E3 [(A2 '())] '())))
  )

(define (test-^)
  (define E1 (^ A2))
  (test-expr U E1 [(A2 '((a b) (b d)))] '((a b) (b d) (a d)))
  (test-expr U E1 [(A2 '((a b) (c d)))] '((a b) (c d)))
  (test-expr U E1 [(A2 '((a b) (b c) (c d) (d a)))]
                  '((a a) (a b) (a c) (a d) (b a) (b b) (b c) (b d)
                    (c a) (c b) (c c) (c d) (d a) (d b) (d c) (d d)))

  (check-exn exn:fail?
    (thunk
     (define E2 (^ A1))
     (test-expr U E2 [(A1 '())] '())))
  )

(define (test-*)
  (define E1 (* A2))
  (test-expr U E1 [(A2 '((a b) (b d)))] '((a b) (b d) (a d) (a a) (b b) (c c) (d d)))
  (test-expr U E1 [(A2 '((a b) (c d)))] '((a b) (c d) (a a) (b b) (c c) (d d)))
  (test-expr U E1 [(A2 '((a b) (b c) (c d) (d a)))]
                  '((a a) (a b) (a c) (a d) (b a) (b b) (b c) (b d)
                    (c a) (c b) (c c) (c d) (d a) (d b) (d c) (d d)))

  (check-exn exn:fail?
    (thunk
     (define E2 (* A1))
     (test-expr U E2 [(A1 '())] '())))
  )

(define (test-<:)
  (define E1 (<: A1 A2))
  (test-expr U E1 [(A1 '((a) (b))) (A2 '((a a) (a b) (b c) (c a)))] '((a a) (a b) (b c)))
  (test-expr U E1 [(A1 '()) (A2 '((a a) (b c)))] '())
  (test-expr U E1 [(A1 '((a))) (A2 '((a c) (c a)))] '((a c)))

  (define E2 (<: A1 A3))
  (test-expr U E2 [(A1 '((a))) (A3 '((a b c) (c d a)))] '((a b c)))

  (define E3 (<: A1 B1))
  (test-expr U E3 [(A1 '((a))) (B1 '((a) (b)))] '((a)))

  (check-exn exn:fail?
    (thunk
     (define E4 (<: A2 A3))
     (test-expr U E4 [(A2 '()) (A3 '())] '())))
  )

(define (test-:>)
  (define E1 (:> A2 A1))
  (test-expr U E1 [(A1 '((a) (b))) (A2 '((a a) (a b) (b c) (c a)))] '((a a) (a b) (c a)))
  (test-expr U E1 [(A1 '()) (A2 '((a a) (b c)))] '())
  (test-expr U E1 [(A1 '((a))) (A2 '((a c) (c a)))] '((c a)))

  (define E2 (:> A3 A1))
  (test-expr U E2 [(A1 '((a))) (A3 '((a b c) (c d a)))] '((c d a)))

  (define E3 (:> B1 A1))
  (test-expr U E3 [(A1 '((a))) (B1 '((a) (b)))] '((a)))

  (check-exn exn:fail?
    (thunk
     (define E4 (:> A3 A2))
     (test-expr U E4 [(A2 '()) (A3 '())] '())))
  )

;; formulas --------------------------------------------------------------------

(define (test-in)
  (define F1 (in A1 B1))
  (test-formula U F1 [(A1 '((a))) (B1 '((a) (b)))] #t)
  (test-formula U F1 [(A1 '((c))) (B1 '((a) (b)))] #f)
  (test-formula U F1 [(A1 '())    (B1 '((a) (b)))] #t)
  (test-formula U F1 [(A1 '((a))) (B1 '())]        #f)

  (define F2 (in A2 B2))
  (test-formula U F2 [(A2 '((a b) (a c))) (B2 '((a b) (a c) (a d)))] #t)
  (test-formula U F2 [(A2 '((a b) (b c))) (B2 '((a b) (a c) (a d)))] #f)

  (check-exn exn:fail?
    (thunk 
      (define F3 (in A1 B2))
      (test-formula U F3 [(A1 '((a))) (B2 '((a b)))] #f)))
  )
  
(define (test-eq)
  (define F1 (= A1 B1))
  (test-formula U F1 [(A1 '((a))) (B1 '((a)))]     #t)
  (test-formula U F1 [(A1 '((a))) (B1 '((a) (b)))] #f)
  (test-formula U F1 [(A1 '())    (B1 '((a) (b)))] #f)
  (test-formula U F1 [(A1 '((a))) (B1 '())]        #f)

  (define F2 (= A2 B2))
  (test-formula U F2 [(A2 '((a b) (a c))) (B2 '((a b) (a c)))] #t)
  (test-formula U F2 [(A2 '((a b) (b c))) (B2 '((a b) (a c)))] #f)

  (check-exn exn:fail?
    (thunk 
      (define F3 (= A1 B2))
      (test-formula U F3 [(A1 '((a))) (B2 '((a b)))] #f)))
  )

(define (test-and)
  (define F (and (in A1 B1) (in A1 C1)))
  (test-formula U F [(A1 '((a))) (B1 '((a) (b))) (C1 '((a) (c)))] #t)
  (test-formula U F [(A1 '((a))) (B1 '((b) (c))) (C1 '((a) (c)))] #f)
  (test-formula U F [(A1 '((a))) (B1 '((a) (b))) (C1 '((b) (c)))] #f)

  (define F2 (and (in A1 B1) (in A1 C1) (in B1 C1)))
  (test-formula U F2 [(A1 '((a))) (B1 '((a) (b))) (C1 '((a) (b) (c)))] #t)
  (test-formula U F2 [(A1 '((a))) (B1 '((b) (c))) (C1 '((a) (c)))]     #f)
  (test-formula U F2 [(A1 '((a))) (B1 '((b)))     (C1 '((a) (b)))]     #f)
  )

(define (test-or)
  (define F (or (in A1 B1) (in A1 C1)))
  (test-formula U F [(A1 '((a))) (B1 '((a) (b))) (C1 '((a) (c)))] #t)
  (test-formula U F [(A1 '((a))) (B1 '((b) (c))) (C1 '((a) (c)))] #t)
  (test-formula U F [(A1 '((a))) (B1 '((d) (b))) (C1 '((b) (c)))] #f)

  (define F2 (or (in A1 B1) (in A1 C1) (in B1 C1)))
  (test-formula U F2 [(A1 '((a))) (B1 '((a) (b))) (C1 '((a) (b) (c)))] #t)
  (test-formula U F2 [(A1 '((a))) (B1 '((b) (c))) (C1 '((a) (c)))]     #t)
  (test-formula U F2 [(A1 '((a))) (B1 '((b)))     (C1 '((c) (b)))]     #t)
  (test-formula U F2 [(A1 '((a))) (B1 '((b)))     (C1 '((c) (d)))]     #f)
  )

(define (test-=>)
  (define F (=> (in A1 B1) (in A1 C1)))
  (test-formula U F [(A1 '((a))) (B1 '((a) (b))) (C1 '((a) (c)))] #t)
  (test-formula U F [(A1 '((a))) (B1 '((b) (c))) (C1 '((a) (c)))] #t)
  (test-formula U F [(A1 '((a))) (B1 '((a) (b))) (C1 '((b) (c)))] #f)

  (check-exn exn:fail?
    (thunk
     (define F2 (=> (in A1 B1) (in A1 C1) (in B1 C1)))
     (test-formula U F2 [(A1 '()) (B1 '()) (C1 '())] #f)))
  )

(define (test-!)
  (define F1 (! (in A1 B1)))
  (test-formula U F1 [(A1 '((a))) (B1 '((a) (b)))] #f)
  (test-formula U F1 [(A1 '((c))) (B1 '((a) (b)))] #t)
  (test-formula U F1 [(A1 '())    (B1 '((a) (b)))] #f)
  (test-formula U F1 [(A1 '((a))) (B1 '())]        #t)

  (define F2 (! (in A2 B2)))
  (test-formula U F2 [(A2 '((a b) (a c))) (B2 '((a b) (a c) (a d)))] #f)
  (test-formula U F2 [(A2 '((a b) (b c))) (B2 '((a b) (a c) (a d)))] #t)

  (check-exn exn:fail?
    (thunk 
      (define F3 (! (in A1 B1) (in A1 C1)))
      (test-formula U F3 [(A1 '()) (B1 '()) (C1 '())] #f)))
  )

(define (test-all)
  (define F1 (all ([x A1]) (in x B1)))
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((a) (b) (c)))] #t)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((a) (c)))]     #f)

  (define F2 (all ([x A1][y B1]) (and (in x C1) (in y C1))))
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a) (b) (c)))] #t)
  (test-formula U F2 [(A1 '((a) (d))) (B1 '((b) (c))) (C1 '((a) (b) (c)))] #f)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (d))) (C1 '((a) (b) (c)))] #f)

  (check-exn exn:fail?
    (thunk
     (define F3 (all ([x A2]) (in x A2)))
     (test-formula U F3 [(A2 '())] #f)))
  )

(define (test-some)
  (define F1 (some ([x A1]) (in x B1)))
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((a) (b) (c)))] #t)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((c) (d)))]     #f)

  (define F2 (some ([x A1][y B1]) (and (in x C1) (in y C1))))
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a) (b) (c)))] #t)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((c)))]         #f)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a)))]         #f)

  (check-exn exn:fail?
    (thunk
     (define F3 (some ([x A2]) (in x A2)))
     (test-formula U F3 [(A2 '())] #f)))

  (define F4 (some (& A1 B1)))
  (test-formula U F4 [(A1 '((a) (b))) (B1 '((a) (c)))] #t)
  (test-formula U F4 [(A1 '((a) (b))) (B1 '((c) (d)))] #f)

  (define F5 (some (& A2 B2)))
  (test-formula U F5 [(A2 '((a a) (b b))) (B2 '((a a) (c c)))] #t)
  (test-formula U F5 [(A2 '((a a) (b b))) (B2 '((c c) (d d)))] #f)
  )

(define (test-no)
  (define F1 (no ([x A1]) (in x B1)))
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((a) (b) (c)))] #f)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((d) (c)))]     #t)

  (define F2 (no ([x A1][y B1]) (and (in x C1) (in y C1))))
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a) (b) (c)))] #f)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((c)))]         #t)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((b) (c))) (C1 '((a)))]         #t)

  (check-exn exn:fail?
    (thunk
     (define F3 (no ([x A2]) (in x A2)))
     (test-formula U F3 [(A2 '())] #f)))

  (define F4 (no (& A1 B1)))
  (test-formula U F4 [(A1 '((a) (b))) (B1 '((a) (c)))] #f)
  (test-formula U F4 [(A1 '((a) (b))) (B1 '((c) (d)))] #t)

  (define F5 (no (& A2 B2)))
  (test-formula U F5 [(A2 '((a a) (b b))) (B2 '((a a) (c c)))] #f)
  (test-formula U F5 [(A2 '((a a) (b b))) (B2 '((c c) (d d)))] #t)
  )

(define (test-one)
  (define F1 (one ([x A1]) (in x B1)))
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((b) (c)))] #t)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((b) (a)))] #f)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((d) (c)))] #f)

  (define F2 (one ([x A1][y B1]) (and (in x C1) (in y C1))))
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((c))) (C1 '((a) (c)))] #t)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((a) (c))) (C1 '((a) (c)))] #f)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((a) (c))) (C1 '((b) (d)))] #f)

  (define F3 (one A1))
  (test-formula U F3 [(A1 '((a)))] #t)
  (test-formula U F3 [(A1 '((a) (b)))] #f)
  (test-formula U F3 [(A1 '())] #f)

  (check-exn exn:fail?
    (thunk
     (define F4 (one ([x A2]) (in x A2)))
     (test-formula U F4 [(A2 '())] #f)))
  )

(define (test-lone)
  (define F1 (lone ([x A1]) (in x B1)))
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((b) (c)))] #t)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((b) (a)))] #f)
  (test-formula U F1 [(A1 '((a) (b))) (B1 '((d) (c)))] #t)

  (define F2 (lone ([x A1][y B1]) (and (in x C1) (in y C1))))
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((c))) (C1 '((a) (c)))] #t)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((a) (c))) (C1 '((a) (c)))] #f)
  (test-formula U F2 [(A1 '((a) (b))) (B1 '((a) (c))) (C1 '((b) (d)))] #t)

  (define F3 (lone A1))
  (test-formula U F3 [(A1 '((a)))] #t)
  (test-formula U F3 [(A1 '((a) (b)))] #f)
  (test-formula U F3 [(A1 '())] #t)

  (check-exn exn:fail?
    (thunk
     (define F4 (lone ([x A2]) (in x A2)))
     (test-formula U F4 [(A2 '())] #f)))
  )

(define (test-contains?)
  (define E1 A2)
  (test-matrix U E1 [(A2 '((a b) (c c) (c d)))]
               (lambda (m) (check-false (matrix/contains? U '(a a) m))))
  (test-matrix U E1 [(A2 '((a b) (c c) (c d)))]
               (lambda (m) (check-true (matrix/contains? U '(a b) m))))
  (test-matrix U E1 [(A2 '((a b) (c c) (c d)))]
               (lambda (m) (check-true (matrix/contains? U '(c c) m))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define expression-tests
  (test-suite
   "expression tests"
   #:before (thunk (printf "----- expression tests -----\n"))
   (test-+)
   (test-&)
   (test--)
   (test-join)
   (test-~)
   (test-->)
   (test-none)
   (test-univ)
   (test-iden)
   (test-set)
   (test-^)
   (test-*)
   (test-<:)
   (test-:>)))

(define formula-tests
  (test-suite
   "formula tests"
   #:before (thunk (printf "----- formula tests -----\n"))
   (test-in)
   (test-eq)
   (test-and)
   (test-or)
   (test-=>)
   (test-!)
   (test-all)
   (test-some)
   (test-no)
   (test-one)
   (test-lone)))

(define matrix-tests
  (test-suite
   "matrix tests"
   #:before (thunk (printf "----- matrix tests -----\n"))
   (test-contains?)))

(module+ test
  (time (run-tests expression-tests))
  (time (run-tests formula-tests))
  (time (run-tests matrix-tests)))
