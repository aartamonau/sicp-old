;; SICP example.
;; Chapter 1.2.6
;; GCD. Simple prime test.
;; Modified in conforming to exercise 1.23

(define (smallest-divisor n)
  (define (next test-divisor)
    (if (= test-divisor 2)
	3
	(+ test-divisor 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))