;; SICP example
;; Fibonacci numbers computation
;; O(n)

(define (fib n)
  (define (fib-iter a b count)
    (if (not (= count 0))
	(fib-iter (+ a b) a (- count 1))
	b))
  (fib-iter 1 0 n))