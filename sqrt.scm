;; Example from SICP.
;; Square root computation using Newton method.
;; Chapter 1.1.7
;; Changed in conforming to exercise 1.46

(load "iterative-improve.scm")

(define (my-sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))