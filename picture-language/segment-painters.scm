(load "drawing.scm")
(load "segments.scm")
(load "frames.scm")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define crossed-rhomb-painter (segments->painter
                               (list (make-segment 0.0 0.0 1.0 1.0)
                                     (make-segment 0.3 0.7 0.7 0.3)
                                     (make-segment 0.0 0.0 0.3 0.7)
                                     (make-segment 0.0 0.0 0.7 0.3)
                                     (make-segment 1.0 1.0 0.3 0.7)
                                     (make-segment 1.0 1.0 0.7 0.3))))

(define big-x-painter (segments->painter
                       (list (make-segment 0.0 0.0 1.0 1.0)
                             (make-segment 1.0 0.0 0.0 1.0))))

(define big-v-painter (segments->painter
                       (list (make-segment 0.0 1.0 0.5 0.0)
                             (make-segment 0.5 0.0 1.0 1.0))))

(define arrow-painter (segments->painter
                       (list (make-segment 0.2 0.2 0.9 0.9)
                             (make-segment 0.9 0.9 0.73 0.8)
                             (make-segment 0.9 0.9 0.8 0.73)
                             (make-segment 0.73 0.8 0.8 0.8)
                             (make-segment 0.8 0.73 0.8 0.8)
                             (make-segment 0.2 0.2 0.2 0.12)
                             (make-segment 0.2 0.2 0.12 0.2)
                             (make-segment 0.22 0.22 0.22 0.14)
                             (make-segment 0.22 0.22 0.14 0.22)
                             (make-segment 0.24 0.24 0.24 0.16)
                             (make-segment 0.24 0.24 0.16 0.24)
                             (make-segment 0.26 0.26 0.26 0.18)
                             (make-segment 0.26 0.26 0.18 0.26)
                             (make-segment 0.28 0.28 0.28 0.2)
                             (make-segment 0.28 0.28 0.2 0.28))))

(define outline-painter (segments->painter
                         (list (make-segment 0.0 0.0 1.0 0.0)
                               (make-segment 1.0 0.0 1.0 1.0)
                               (make-segment 1.0 1.0 0.0 1.0)
                               (make-segment 0.0 1.0 0.0 0.0))))

(define star-painter (segments->painter
                      (list (make-segment 0.15 0.0 0.5 1.0)
                            (make-segment 0.85 0.0 0.5 1.0)
                            (make-segment 0.0 0.6 1.0 0.6)
                            (make-segment 0.15 0.0 1.0 0.6)
                            (make-segment 0.85 0.0 0.0 0.6))))

(define david-star-painter (segments->painter
                            (list (make-segment 0.5 1.0 0.0 0.25)
                                  (make-segment 0.5 1.0 1.0 0.25)
                                  (make-segment 0.0 0.25 1.0 0.25)
                                  (make-segment 0.5 0.0 0.0 0.75)
                                  (make-segment 0.5 0.0 1.0 0.75)
                                  (make-segment 0.0 0.75 1.0 0.75))))
