#lang racket

(require "../lang/bounds.rkt" "../lang/universe.rkt" 
         "../engine/interpretation.rkt" "../engine/matrix.rkt" "tuple.rkt"
         (only-in "../lang/ast.rkt" relation-arity relation-name declare-relation)
         (only-in rosette && => <=>))
(provide generate-sbp)


;; utilities -------------------------------------------------------------------
(define (bound->tupleset U bnd accessor)
  (define t->i (curry tuple->idx U))
  (tupleset (relation-arity (bound-relation bnd))
            (list->set (map t->i (accessor bnd)))))
(define (constants bounds)
  (define C (mutable-set))
  (define U (bounds-universe bounds))
  (for ([b (bounds-entries bounds)])
    (define arity (relation-arity (bound-relation b)))
    (unless (null? (bound-lower b))
      (set-add! C (bound->tupleset U b bound-lower)))
    (unless (null? (bound-upper b))
      (set-add! C (bound->tupleset U b bound-upper))))
  (sort (set->list C) < #:key set-count))


;; symmetry detection ----------------------------------------------------------
; return a set of sets that partition the atoms of (bounds-universe bounds)
; according to their symmetries
(define (partition bounds)
  (define C (constants bounds))
  (define parts (list (tupleset 1 (list->set (range (universe-size (bounds-universe bounds)))))))
  (for/fold ([S parts]) ([c C])
    (refine-partitions (bounds-universe bounds) S c)))

(define (refine-partitions U parts c)
  (if (= (tupleset-arity c) 1)
      (refine-partitions-unary U parts c)
      (refine-partitions-nary U parts c)))

(define (refine-partitions-unary U parts c)
  (for/fold ([S '()]) ([part parts])
    (define int (set-intersect part c))
    (if (and (not (set-empty? int)) (< (set-count int) (set-count part)))
        (append (list int (set-subtract part int)) S)
        (cons part S))))
            
(define (refine-partitions-nary U parts c)
  (define usize (universe-size U))
  (define arity (tupleset-arity c))

  (define otherColumns '())
  (define firstColFactor (expt usize (- arity 1)))
  (define firstCol (tupleset 1 (for/set ([t c]) (quotient t firstColFactor))))
  (set! parts (refine-partitions-unary U parts firstCol))

  (define idenFactor (quotient (- 1 firstColFactor) (- 1 usize)))
  (define newParts
    (for/fold ([S '()]) ([part parts])
      (if (set-empty? (set-intersect firstCol part))
          (cons part S)
          (let ([range2domain (make-hash)])
            (for ([atom part])
              (define atomRange
                (tupleset 1
                 (for/set ([p c] #:when (and (<= (* atom firstColFactor) p)
                                             (<= p (* (add1 atom) firstColFactor))))
                   (remainder p firstColFactor))))
              (define atomDomain
                (hash-ref! range2domain atomRange (thunk (tupleset 1 (set)))))
              (hash-set! range2domain atomRange (set-add atomDomain atom)))
            (define idenPartition (tupleset 1 (set)))
            (for ([(key value) range2domain])
              (cond [(and (= (set-count value) 1) (= (set-count key) 1)
                          (= (tupleset-min key) (* (tupleset-min value) idenFactor)))
                     (set! idenPartition (set-add idenPartition (tupleset-min value)))]
                    [else
                     (set! S (cons value S))
                     (set! otherColumns (cons key otherColumns))]))
            (unless (set-empty? idenPartition)
              (set! S (cons idenPartition S)))
            S))))
  (for/fold ([S newParts]) ([otherCol otherColumns])
    (refine-partitions U S otherCol)))


;; symmetry breaking -----------------------------------------------------------
(define (constant-bound? bnd)
  (equal? (bound-lower bnd) (bound-upper bnd)))
(struct relation-parts (rel reps) #:transparent)
(define (relation-part<? o1 o2)
  (match-define (list r1 r2) (map relation-parts-rel (list o1 o2)))
  (if (= (relation-arity r1) (relation-arity r2))
      (string<? (relation-name r1) (relation-name r2))
      (< (relation-arity r1) (relation-arity r2))))

; Generate a predicate over the booleans in `interp` that rules out symmetries
; in the given bounds
(define (generate-sbp interp bounds)
  (define symmetries (partition bounds))
  (define U (bounds-universe bounds))
  (define relParts
    (sort
     (for/list ([bnd (bounds-entries bounds)] #:unless (constant-bound? bnd))
       (relation-parts
        (bound-relation bnd)
        (tupleset 1 (for*/fold ([reps (set)])
                               ([t (bound-upper bnd)][a t][symm symmetries])
                      (if (set-member? symm (tuple->idx U (list a)))
                          (set-add reps (tupleset-min symm))
                          reps)))))
     relation-part<?))
  (define sbps
    (for/list ([sym symmetries] #:when #t [prevIndex sym][curIndex (sequence-tail sym 1)])
      (define original '())
      (define permuted '())
      (for ([rparts relParts]
            #:when (set-member? (relation-parts-reps rparts) (tupleset-min sym)))
        (define m (cdr (assoc (relation-parts-rel rparts) (interpretation-entries interp))))
        (define arity (relation-arity (relation-parts-rel rparts)))
        (for ([(entry i) (in-indexed (matrix-entries m))] #:unless (false? entry))
          (define permIndex (permutation U arity i prevIndex curIndex))
          (define permValue (list-ref (matrix-entries m) permIndex))
          (unless (or (= permIndex i) (atSameIndex original permValue permuted entry))
            (set! original (cons entry original))
            (set! permuted (cons permValue permuted)))))
      (leq (reverse original) (reverse permuted))))
  (apply && sbps))

(define (permutation U arity iT i0 i1)
  (define usize (universe-size U))
  (let loop ([permIndex 0][u 1][arity arity][iT iT])
    (if (> arity 0)
        (let ()
          (define iA (remainder iT usize))
          (define inc (cond [(= iA i0) (* i1 u)]
                            [(= iA i1) (* i0 u)]
                            [else (* iA u)]))
          (loop (+ permIndex inc) (* u usize) (sub1 arity) (quotient iT usize)))
        permIndex)))

(define (atSameIndex l0 v0 l1 v1)
  (for/or ([x0 l0][x1 l1]) (and (equal? x0 v0) (equal? x1 v1))))

(define (leq l0 l1)
  (define prevEquals #t)
  (apply &&
         (for/list ([v0 l0][v1 l1])
           (begin0
             (=> prevEquals (=> v0 v1))
             (set! prevEquals (&& prevEquals (<=> v0 v1)))))))
