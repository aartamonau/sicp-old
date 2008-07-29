;; SICP exercise 1.42
;; Composition of two functions

(define (compose f g)
  (lambda (x) (f (g x))))