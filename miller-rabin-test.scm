;; SICP exercise 1.28
;; Miller-Rabin test

(define (fast-prime? n times)
  (define (expmod base exp m)
    (define result
      (cond ((= exp 0) 1)
	    ((even? exp)
	     (remainder (square (expmod base (/ exp 2) m))
			m))
	    (else
	     (remainder (* base (expmod base (- exp 1) m))
			m))))
    (if (and (= result 1)
	     (not (= base 1))
	     (not (= base (- n 1)))
	     (= exp 2))
	0
	result))
	     
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (miller-rabin-test n)
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) #t)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else #f)))