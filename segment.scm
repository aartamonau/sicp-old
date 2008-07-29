;; SICP ex. 2.2
;; Segments representation.

(load "point.scm")

(define (make-segment s e)
  (cons s e))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
	(end (end-segment seg)))
    (make-point (/ (+ (x-point start)
		      (x-point end))
		   2)
		(/ (+ (y-point start)
		      (y-point end))
		   2))))

(define (lenght-segment seg)
  (let ((end (end-segment seg))
	(start (start-segment seg)))
    (sqrt (+ (square (- (x-point end) (x-point start)))
	     (square (- (y-point end) (y-point start)))))))