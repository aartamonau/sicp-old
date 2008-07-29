;; SICP example.
;; Chapter 1.2.5
;; GCD computation.
;; With my changes.
		  
(define (gcd a b)
  (define (gcd-positive a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))
  (gcd-positive (abs a)
		(abs b)))