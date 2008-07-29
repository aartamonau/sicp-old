;; SICP exercise 1.18
;; Fast multiplication in iterative form

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast-mul a b)
  (define (fast-mul-iter a b correction)
    (if (= b 0)
	correction
	(if (even? b)
	    (fast-mul-iter (double a)
			   (halve b)
			   correction)
	    (fast-mul-iter a
			   (- b 1)
			   (+ correction a)))))
  (fast-mul-iter a b 0))