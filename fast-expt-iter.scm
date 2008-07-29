;; SICP exercise 1.16
;; Fast exptonentiation in iterative form.

(define (fast-expt base exponent)
  (define (fast-expt-iter base exponent scale)
    (if (= exponent 0)
	scale
	(if (even? exponent)
	    (fast-expt-iter (square base)
			    (/ exponent 2)
			    scale)
	    (fast-expt-iter base
			    (- exponent 1)
			    (* scale base)))))
  (fast-expt-iter base exponent 1.0))
	    
