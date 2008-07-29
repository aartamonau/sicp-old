;; SICP exercise 1.32
;; Accumulate-abstraction. Recursive form.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))