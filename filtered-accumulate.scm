;; SICP exercise 1.33.
;; Filtered accumulate.

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
	  ((filter a) (iter (next a) (combiner (term a) result)))
	  (else (iter (next a) result))))
  (trace-entry iter)
  (iter a null-value))