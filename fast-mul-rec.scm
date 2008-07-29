;; SICP exercise 1.17
;; Fast multiplication of two numbers in recursive form

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= b 1) a)
	((even? b) (double (mul a (halve b))))
	(else (+ (mul a (- b 1))
		 a))))
  