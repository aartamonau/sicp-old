;; SICP exercise 1.44
;; Smooth of function.
;; Repeated smooth of function

(load "repeated.scm")

(define dx 0.001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (repeated-smooth f times)
  ((repeated smooth times) f))