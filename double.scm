;; SICP exercise 1.41
;; Doubling procedure.

(define (double procedure)
  (lambda (x) (procedure (procedure x))))