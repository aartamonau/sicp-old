; SICP ex. 2.47

(module frames scheme
  (require "vectors.ss")
  (provide make-frame
           origin-frame edge1-frame edge2-frame
           frame-coord-map)

  (define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

  (define (origin-frame frame)
    (car frame))

  (define (edge1-frame frame)
    (cadr frame))

  (define (edge2-frame frame)
    (caddr frame))

  (define (frame-coord-map frame)
    (lambda (v)
      (add-vect
       (origin-frame frame)
       (add-vect (scale-vect (xcor-vect v)
                             (edge1-frame frame))
                 (scale-vect (ycor-vect v)
                             (edge2-frame frame)))))))
