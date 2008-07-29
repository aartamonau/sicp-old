;; SICP exercise 1.32
;; Accumulate-abstraction. Iterative form.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))