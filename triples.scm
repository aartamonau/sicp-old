; SICP ex. 2.41

(load "flatmap.scm")
(load "enumerate-interval.scm")

(define (triple-exact-sum n sum)
  (define (unique-triples n)
    (flatmap (lambda (i)
	       (flatmap (lambda (j)
			  (map (lambda (k) (list i j k))
			       (enumerate-interval 1 (- j 1))))
			(enumerate-interval 1 (- i 1))))
	     (enumerate-interval 1 n)))
  (define (exact-sum? triple)
    (= sum (+ (car triple) (cadr triple) (caddr triple))))
  (filter exact-sum? (unique-triples n)))
		 