;; SICP exercise 1.37
;; Finite continued fraction computation. Iterative form.

(define (cont-frac n d k)
  (define (iter result k)
    (if (= k 0)
	result
	(iter (/ (n k)
		 (+ (d k) result))
	      (- k 1))))
  (iter 0 k))