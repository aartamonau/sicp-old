;; SICP exercise 1.46

(define (iterative-improve check? improve-method)
  (define (iter value)
    (if (check? value)
	value
	(iter (improve-method value))))
  (lambda(x) (iter x)))