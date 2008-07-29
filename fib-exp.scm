;; SICP example.
;; Fibonacci numbers computation.
;; O(fi ^ n), fi = (1 + sqrt(5)) / 2

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))