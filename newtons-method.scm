;; SICP example.
;; Newton's method.
;; Chapter 1.3.4

(load "fixed-point.scm")

(define (newtons-method g guess)
  (define dx 0.00001)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
	 dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))
