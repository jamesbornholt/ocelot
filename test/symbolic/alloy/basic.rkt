#lang racket

(require ocelot ocelot/lib/alloy rackunit)

; declare signatures
(define-sig Platform)
(define-sig Man
  [ceiling : Platform]
  [floor : Platform]
  [between : Platform Platform])

; should be trivially sat
(check-true
 (alloy-run (in Man Man) (scope 5)))

; can be made true if Man is empty
(alloy-fact (in Man Platform))
(check-true
 (alloy-run (in Man Man) (scope 5)))

; can't be true if Man in Platform
(alloy-fact (some Man))
(check-false
 (alloy-run (in Man Man) (scope 5)))

(clear-alloy-facts!)

; (in Man Platform) => (no Man)
(alloy-fact (in Man Platform))
(check-true
 (alloy-check (no Man) (scope 5)))
