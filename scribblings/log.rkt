#lang racket

(require (for-label racket)
         racket/sandbox racket/serialize scribble/eval)
(provide make-log-evaluator logfile opaque)


;; A stripped-down of Rosette's rosette/doc/guide/scribble/util/lifted.rkt
;; used to log all evaluations to a text file.

(define (make-lang-evaluator lang [eval-limits #f])
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-path-permissions `((execute ,(byte-regexp #".*")))]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits eval-limits])
     (make-evaluator lang)))

(define (logfile root [filename "log"])
  (build-path root (format "~a.txt" filename)))

(define (serialize-for-logging v)
  (match v
    [(or (? boolean?) (? number?) (? string?) (? void?)) v]
    [(? box?) (box (serialize-for-logging (unbox v)))]
    [(? pair?) (cons (serialize-for-logging (car v)) (serialize-for-logging (cdr v)))]
    [(? list?) (map serialize-for-logging v)]
    [(? vector?) (list->vector (map serialize-for-logging (vector->list v)))]
    [(? struct?)
     (let ([output-str (open-output-string)])
       (display v output-str)
       (opaque (get-output-string output-str)))]
    [_ v]))

(serializable-struct opaque (str)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (opaque-str self)))])

(define (serializing-evaluator evaluator)
  (lambda (expr) (serialize-for-logging (evaluator expr))))

(define (make-log-evaluator logfile lang [eval-limits #f])  
  (if (file-exists? logfile)
      (make-log-based-eval logfile 'replay)
      (parameterize ([current-eval (serializing-evaluator (make-lang-evaluator lang eval-limits))])
        (make-log-based-eval logfile 'record))))
