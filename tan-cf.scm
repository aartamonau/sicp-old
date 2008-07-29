;; SICP exersise 1.39
;; Computation of tangent using Lambert's formula

(load "cont-frac-iter.scm")

(define (tan-cf x k)
  (let ((square-of-x (square x)))
       (cont-frac (lambda(i) (if (= i 1)
				 x
				 (- square-of-x)))
		  (lambda(i) (- (* 2 i) 1))
		  k)))
