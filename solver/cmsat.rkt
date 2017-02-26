#lang racket

(require racket/runtime-path
         ffi/unsafe ffi/unsafe/define ffi/unsafe/cvector
         rackunit
         "solver.rkt")
(provide make-cmsat-solver (all-from-out "solver.rkt"))

(define-runtime-path libcryptominisat5 "libcryptominisat5")

(define-ffi-definer define-cmsat (ffi-lib libcryptominisat5))

; typedef struct c_Lit { uint32_t x; } c_Lit;
(define-cstruct _Lit ([x _uint32]))
; typedef struct c_lbool { uint8_t x; } c_lbool;
(define-cstruct _lbool ([x _uint8]))
; typedef struct slice_Lit { const c_Lit* vals; size_t num_vals; } slice_Lit;
(define-cstruct _slice_Lit ([vals (_cpointer _Lit)][num_vals _size]))
; typedef struct slice_lbool { const c_lbool* vals; size_t num_vals; } slice_lbool;
(define-cstruct _slice_lbool ([vals (_cpointer _lbool)][num_vals _size]))

; #define L_TRUE (0u)
(define L_TRUE 0)
; #define L_FALSE (1u)
(define L_FALSE 1)
; #define L_UNDEF (2u)
(define L_UNDEF 2)

; typedef struct SATSolver SATSolver;
(define _SATSolver* (_cpointer 'SATSolver))

; CMS_DLL_PUBLIC SATSolver* cmsat_new(void) NOEXCEPT;
(define-cmsat cmsat_new (_fun -> _SATSolver*))
; CMS_DLL_PUBLIC void cmsat_free(SATSolver* s) NOEXCEPT;
(define-cmsat cmsat_free (_fun _SATSolver* -> _void))

; CMS_DLL_PUBLIC unsigned cmsat_nvars(const SATSolver* self) NOEXCEPT;
(define-cmsat cmsat_nvars (_fun _SATSolver* -> _uint))
; CMS_DLL_PUBLIC bool cmsat_add_clause(SATSolver* self, const c_Lit* lits, size_t num_lits) NOEXCEPT;
(define-cmsat cmsat_add_clause (_fun _SATSolver* _pointer _size -> _stdbool))
; CMS_DLL_PUBLIC bool cmsat_add_xor_clause(SATSolver* self, const unsigned* vars, size_t num_vars, bool rhs) NOEXCEPT;
(define-cmsat cmsat_add_xor_clause (_fun _SATSolver* _pointer _size _stdbool -> _stdbool))
; CMS_DLL_PUBLIC void cmsat_new_vars(SATSolver* self, const size_t n) NOEXCEPT;
(define-cmsat cmsat_new_vars (_fun _SATSolver* _size -> _void))

; CMS_DLL_PUBLIC c_lbool cmsat_solve(SATSolver* self) NOEXCEPT;
(define-cmsat cmsat_solve (_fun _SATSolver* -> _lbool))
; CMS_DLL_PUBLIC c_lbool cmsat_solve_with_assumptions(SATSolver* self, const c_Lit* assumptions, size_t num_assumptions) NOEXCEPT;
(define-cmsat cmsat_solve_with_assumptions (_fun _SATSolver* _pointer _size -> _lbool))
; CMS_DLL_PUBLIC slice_lbool cmsat_get_model(const SATSolver* self) NOEXCEPT;
(define-cmsat cmsat_get_model (_fun _SATSolver* -> _slice_lbool))
; CMS_DLL_PUBLIC slice_Lit cmsat_get_conflict(const SATSolver* self) NOEXCEPT;
(define-cmsat cmsat_get_conflict (_fun _SATSolver* -> _slice_Lit))

; CMS_DLL_PUBLIC void cmsat_set_num_threads(SATSolver* self, unsigned n) NOEXCEPT;
(define-cmsat cmsat_set_num_threads (_fun _SATSolver* _uint -> _void))

(define (new_lit var neg)
  (make-Lit (bitwise-ior (arithmetic-shift var 1) (if neg 1 0))))
(define (lit->Lit lit)
  (bitwise-ior (arithmetic-shift (sub1 (abs lit)) 1) (if (< lit 0) 1 0)))


(struct cmsat (solver [num-vars #:mutable] [clause #:mutable] [model #:mutable])
  #:methods gen:solver
  [(define (assert-clause self cl)
     (when (> (length cl) (cvector-length (cmsat-clause self)))
       (set-cmsat-clause! self (make-cvector _Lit (* 2 (length cl)))))
     (define clause-vec (cmsat-clause self))
     (for ([(lit i) (in-indexed cl)])
       (when (> (abs lit) (cmsat-num-vars self))
         (define delta (- (abs lit) (cmsat-num-vars self)))
         (cmsat_new_vars (cmsat-solver self) delta)
         (set-cmsat-num-vars! self (abs lit)))
       (set-Lit-x! (cvector-ref clause-vec i) (lit->Lit lit)))
     (cmsat_add_clause (cmsat-solver self) (cvector-ptr clause-vec) (length cl))
     (void))
   (define (solve self)
     (set-cmsat-model! self #f)  ; clear out the model
     (equal? (lbool-x (cmsat_solve (cmsat-solver self))) L_TRUE))
   (define (get-value self var)
     (when (false? (cmsat-model self))
       (set-cmsat-model! self (cmsat_get_model (cmsat-solver self))))
     (unless (and (> var 0) (<= var (slice_lbool-num_vals (cmsat-model self))))
       (raise-argument-error 'get-value (format "index in [1,~v]" (slice_lbool-num_vals (cmsat-model self))) var))
     (equal? (lbool-x (ptr-ref (slice_lbool-vals (cmsat-model self)) _lbool (sub1 var))) L_TRUE))
   (define (shutdown self)
     (cmsat_free (cmsat-solver self)))])

(define (make-cmsat-solver [vars 0])
  (define solver (cmsat_new))
  (when (> vars 0)
    (cmsat_new_vars solver vars))
  (cmsat solver vars (make-cvector _Lit 100) #f))

(module+ test
  (define s1 (make-cmsat-solver 3))
  (assert-clause s1 '(1))
  (assert-clause s1 '(-2))
  (assert-clause s1 '(-1 2 3))
  (check-true (solve s1))
  (check-true (get-value s1 1))
  (check-false (get-value s1 2))
  (check-true (get-value s1 3))
  (assert-clause s1 '(-3))
  (check-false (solve s1))
  (shutdown s1)

  (define s2 (make-cmsat-solver))
  (assert-clause s2 '(1))
  (assert-clause s2 '(-2))
  (assert-clause s2 '(-1 2 3))
  (check-true (solve s2))
  (check-true (get-value s2 1))
  (check-false (get-value s2 2))
  (check-true (get-value s2 3))
  (assert-clause s2 '(-3))
  (check-false (solve s2))
  (shutdown s2)
  )
