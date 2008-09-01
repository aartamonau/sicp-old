(module complex scheme
  (provide add-complex sub-complex
           mul-complex div-complex
           make-from-real-imag
           make-from-mag-ang
           real-part imag-part
           magnitude angle)

  (define (square x) (* x x))

  (define (make-from-real-imag x y)
    (lambda (op)
      (cond ((eq? op 'real-part) x)
            ((eq? op 'imag-part) y)
            ((eq? op 'magnitude)
             (sqrt (+ (square x) (square y))))
            ((eq? op 'angle) (atan y x))
            (else
             (error "Unknown operation. [MAKE-FROM-REAL-IMAG]" op)))))

  (define (make-from-mag-ang r a)
    (lambda (op)
      (cond ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)
            ((eq? op 'real-part) (* r (cos a)))
            ((eq? op 'imag-part) (* r (sin a)))
            (else
             (error "Unknown operation. [MAKE-FROM-MAG-ANG]" op)))))
  
  (define (real-part z) (z 'real-part))
  (define (imag-part z) (z 'imag-part))
  (define (magnitude z) (z 'magnitude))
  (define (angle z) (z 'angle))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2)))))
