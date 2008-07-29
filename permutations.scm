; SICP chapeter 2.2.3

(load "flatmap.scm")

(define (permutations s)
  (define (remove x s)
    (filter (lambda (y) (not (= y x))) s))
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))