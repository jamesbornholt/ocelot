#lang racket

(require ocelot ocelot/lib/alloy rackunit)

; This file implements the ceilingsAndFloors.als Alloy demo
; (found in Alloy's sample models under examples/toys/).


; sig Platform {}
; sig Man {ceiling, floor: Platform}
(define-sig Platform)
(define-sig Man
  [ceiling : Platform]
  [floor   : Platform])

; pred Above[m, n: Man] {m.floor = n.ceiling}
(define (Above m n) (= (join m floor) (join n ceiling)))

; fact PaulSimon {all m: Man | some n: Man | n.Above[m]}
(define PaulSimon
  (all ([m Man]) (some ([n Man]) (Above n m))))
(alloy-fact PaulSimon)

; assert BelowToo { all m: Man | some n: Man | m.Above[n] }
(define BelowToo (all ([m Man]) (some ([n Man]) (Above m n))))

; check BelowToo for 2 expect 1
(check-false
 (alloy-check BelowToo (scope 2)))

; pred Geometry {no m: Man | m.floor = m.ceiling}
(define Geometry (no ([m Man]) (= (join m floor) (join m ceiling))))

; assert BelowToo' { Geometry => (all m: Man | some n: Man | m.Above[n]) }
(define BelowToo^
  (=> Geometry (all ([m Man]) (some ([n Man]) (Above m n)))))

; check BelowToo' for 2 expect 0
(check-true
 (alloy-check BelowToo^ (scope 2)))
; check BelowToo' for 3 expect 1
(check-false
 (alloy-check BelowToo^ (scope 3)))

; pred NoSharing {
;  no m,n: Man | m!=n && (m.floor = n.floor || m.ceiling = n.ceiling)
; }
(define NoSharing
  (no ([m Man][n Man])
      (and (! (= m n))
           (or (= (join m floor) (join n floor))
               (= (join m ceiling) (join n ceiling))))))

; assert BelowToo'' { NoSharing => (all m: Man | some n: Man | m.Above[n]) }
(define BelowToo^^
  (=> NoSharing (all ([m Man]) (some ([n Man]) (Above m n)))))

; check BelowToo'' for 6 expect 0
(check-true
 (time (alloy-check BelowToo^^ (scope 6))))
(check-true
 (time (alloy-check BelowToo^^ (scope 7))))
#|
(check-true
 (time (alloy-check BelowToo^^ (scope 8))))
(check-true
 (time (alloy-check BelowToo^^ (scope 9))))
(check-true
 (time (alloy-check BelowToo^^ (scope 10))))
|#
