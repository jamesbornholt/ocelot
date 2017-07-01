#lang rosette

(require ocelot ocelot/engine/symmetry rackunit)

; Test the symmetry breaking predicate generator
; using the example in Figure 3-4 here:
;   https://homes.cs.washington.edu/~emina/doc/kodkod.phd.pdf


; Bounds:
;   {} ⊆ File ⊆ {f0, f1, f2}
;   {} ⊆ Dir ⊆ {d0, d1}
;   {} ⊆ Root ⊆ {d0, d1}
;   {} ⊆ contents ⊆ {d0, d1} × {d0, d1, f0, f1, f2}

(define File (declare-relation 1 "File"))
(define Dir (declare-relation 1 "Dir"))
(define Root (declare-relation 1 "Root"))
(define contents (declare-relation 2 "contents"))

(define dirs '(d0 d1))
(define files '(f0 f1 f2))
(define U (universe (append dirs files)))

(define bnds
  (bounds U (list (make-upper-bound File (map list files))
                  (make-upper-bound Dir (map list dirs))
                  (make-upper-bound Root (map list dirs))
                  (make-product-bound contents dirs (append dirs files)))))
(define interp (instantiate-bounds bnds))


; Constraints:
;   one Root
;   Root ⊆ Dir
;   contents ⊆ Dir → (Dir ∪ File)
;   ∀ d: Dir | ¬(d ⊆ d.ˆcontents)
;   (File ∪ Dir) ⊆ Root.*contents

(define F
  (and (one Root)
       (in Root Dir)
       (in contents (-> Dir (+ Dir File)))
       (all ([d Dir]) (not (in d (join d (^ contents)))))
       (in (+ File Dir) (join Root (* contents)))))

(define F* (interpret* F interp))


; Generate symmetry breaking predicate

(define sbp (generate-sbp interp bnds))


(module+ test
  ; 1. formula should be satisfiable
  (check-true (sat? (solve (assert F*))))

  ; 2. symmetry-breaking predicate should be satisfiable but not vacuous
  (check-true (sat? (solve (assert sbp))))
  (check-true (sat? (solve (assert (! sbp)))))

  ; 3. formula should remain satisfiable with the symmetry-breaking predicate
  (check-true (sat? (solve (assert (and F* sbp))))))
