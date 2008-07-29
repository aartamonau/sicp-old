;; SICP exercise 1.37
;; Finite continued fraction computation. Recursive form.

(define (cont-frac n d k)
  (define (recursion step)
    (if (> step k)
	0
	(/ (n step) (+ (d step)
		       (recursion (+ step 1))))))
  (recursion 1))