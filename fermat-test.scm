;; SICP example.
;; Chapter 1.2.6
;; Fermat prime test.

(define (fast-prime? n times)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (remainder (square (expmod base (/ exp 2) m))
		      m))
	  (else
	   (remainder (* base (expmod base (- exp 1) m))
		      m))))
  (define (try-it a)
    (= (expmod a n n) a))
  (define (fermat-test n)
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))