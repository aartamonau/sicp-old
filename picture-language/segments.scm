; SICP ex. 2.48

(load "vectors.scm")

(define (make-segment x1 y1 x2 y2)
  (list (make-vect x1 y1)
	(make-vect x2 y2)))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))