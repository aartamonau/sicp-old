(module complex scheme
  (require "complex-abstraction-layer.ss" "../tag-system/tag-system.ss" "../operations-table/operations-table.ss")
  (provide install-complex-package)

  (define (install-complex-package)
    (define (make-from-real-imag x y)
      ((get 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang x y)
      ((get 'make-from-mag-ang 'polar) x y))
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
                         (- (angle z1) (angle z2))))

    ;; interface
    (define (tag z) (attach-tag 'complex z))

    (put 'add '(complex complex)
         (lambda (x y) (tag (add-complex x y))))
    (put 'sub '(complex complex)
         (lambda (x y) (tag (sub-complex x y))))
    (put 'mul '(complex complex)
         (lambda (x y) (tag (mul-complex x y))))
    (put 'div '(complex complex)
         (lambda (x y) (tag (div-complex x y))))
    (put 'equ? '(complex complex)
         (lambda (x y) (and (eqv? (real-part x) (real-part y))
                            (eqv? (imag-part x) (imag-part y)))))
    (put '=zero? '(complex)
         (lambda (x) (and (eqv? (magnitude x) 0))))

    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (x y) (tag (make-from-mag-ang x y))))
    'done))
