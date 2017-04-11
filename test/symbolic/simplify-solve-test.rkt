#lang racket

(require rackunit ocelot/lang/ast ocelot/lib/simplify-solve)


; Some memory model relations that have rich type information:
; * Reads + Writes = MemoryEvent
; * Atomics ⊂ Writes
; * no Reads & Writes
; * po ⊂ (-> MemoryEvent MemoryEvent)
(define Atomics (declare-relation 1 "Atomics"))
(define Reads (declare-relation 1 "Reads"))
(define Writes (declare-relation 1 "Writes"))
(define MemoryEvent (declare-relation 1 "MemoryEvent"))
(define po (declare-relation 2 "po"))


(define (same-cost e1 e2)
  (= (ast-cost e1) (ast-cost e2)))


(module+ test  
  (define ex1 (- Atomics Writes))
  (check same-cost
         (simplify/solve ex1 (in Atomics Atomics))
         (- Atomics Writes))
  (check same-cost
         (simplify/solve ex1 (in Atomics Writes))
         none)


  (define ex2 (+ (- Atomics Writes) (& Reads Writes)))
  (check same-cost
         (simplify/solve ex2 (in Atomics Writes))
         (& Reads Writes))
  (check-equal?
   (simplify/solve ex2 (and (in Atomics Writes) (no (& Reads Writes))))
   none)


  (define ex3 (- (- MemoryEvent Reads) (- Writes Reads)))
  (check same-cost
         (simplify/solve ex3 (and (in (+ Reads Writes) MemoryEvent)
                                  (no (& Writes Reads))))
         (- Writes (- MemoryEvent Reads)))


  (define ex4  ; ppo for an x86 memory model
    (& po (+ (- (- po (-> Reads Atomics))
                (-> (- Writes Atomics) Reads))
             (-> (- Reads Atomics) (& Atomics (- Writes Reads))))))
  (check same-cost
         (time (simplify/solve ex4 (and (no (& Reads Writes))
                                        (in Atomics Writes))))
         (- po (-> (- Writes Atomics) Reads)))
  
  )