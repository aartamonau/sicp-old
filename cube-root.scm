;; SICP exercise 1.8
;; Computation of cube root using

(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess guess) x)) 0.0001))
  (define (improve guess)
    (/ (+ (/ x (square guess))
	  (* 2 guess))
       3))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
	guess
	(cube-root-iter (improve guess))))
  (cube-root-iter 1.0))