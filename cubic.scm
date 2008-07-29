;; SICP exercise 1.40
;; Third order equality's roots.

(define (cubic a b c d)
  (lambda (x) (+ (* a x x x)
		 (* b x x)
		 (* c x)
		 d)))