#lang racket

(require racket/generic)
(provide (all-defined-out))

(define-generics solver
  (assert-clause solver clause)
  (solve solver)
  (get-value solver var)
  (shutdown solver))
