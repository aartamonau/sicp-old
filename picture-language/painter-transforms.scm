;; some other indirect definition of transforms can be found in "indirect-painter-transforms.scm"

(load "frames.scm")
(load "higher-order-operations.scm")

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((map (frame-coord-map frame))
           (new-origin (map origin)))
      (painter
       (make-frame new-origin
                   (sub-vect (map corner1) new-origin)
                   (sub-vect (map corner2) new-origin))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (shrink-to-upper-left painter)
  (transform-painter painter
                     (make-vect 0.0 0.5)
                     (make-vect 0.5 0.5)
                     (make-vect 0.0 1.0)))

(define (shrink-to-lower-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.5 0.5)))

(define (shrink-to-lower-left painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.5 0.0)
                     (make-vect 0.0 0.5)))

(define (identity painter)
  painter)

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
;; left, right
(define (beside painter1 painter2)
  (let* ((split-point (make-vect 0.5 0.0))
         (paint-left
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             split-point
                             (make-vect 0.0 1.0)))
         (paint-right
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.0)
                             (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

;; right, left
(define (beside-rev painter1 painter2)
  (beside painter2 painter1))

;; bottom, top
(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-down
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point))
         (paint-up
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-down frame)
      (paint-up frame))))

;; top, bottom
(define (below-rev painter1 painter2)
  (below painter2 painter1))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter
                (below smaller smaller)))))

(define (left-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (left-split painter (- n 1))))
        (beside (below smaller smaller)
                painter))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter
               (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below (flip-vert painter2) painter2)))

(define (put-on background foreground)
  (lambda (frame)
    (background frame)
    (foreground frame)))

;; uses higher order operations only to train myself
(define (turned-out-square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))