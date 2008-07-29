;; SICP ex. 2.3
;; Rectangles' representation.

(load "segment.scm")

(define (make-rectangle base width)
  (cons (base width)))

(define (widht-rectangle rec)
  (cdr rec))

(define (lenght-rectangle rec)
  (length-segment (car rec)))

(define (square-rectangle rec)
  (* (length-segment base)
     widht))

(define (perimeter-rectangle rec)
  (* 2 (+ (length-segment base)
	  width)))

(define (vertexes-rectangle rec)
  (list (start-segment (car rec))
	(make-point (