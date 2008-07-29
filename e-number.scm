;; SICP exersise 1.38
;; Computation of e value using Euler continued fraction.

(load "cont-frac-iter.scm")

(define (e-number k)
  (+ 2
     (cont-frac (lambda(x) 1.0)
		(lambda(x)
		  (if (= (remainder (- x 2) 3) 0)
		      (+ 2 (* 2
			      (/ (- x 2) 3)))
		      1))
		k)))
