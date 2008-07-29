;; SICP example.
;; Fixed point calculation.
;; Chapter 1.3.4
;; Changed in conforming to 1.46

(load "iterative-improve.scm")

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iterative-improve (lambda (guess) (let ((next (f guess)))
					(< (abs (- next guess))
					   tolerance)))
		      (lambda (guess) (f guess))) first-guess))