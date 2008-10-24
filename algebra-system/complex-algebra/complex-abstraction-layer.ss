(module complex-abstraction-layer scheme
  (require "complex-polar.ss" "complex-rectangular.ss" "../operations-table/operations-table.ss" "../tag-system/tag-system.ss"
           "../generic/apply.ss")
  (provide real-part imag-part magnitude angle
           make-from-real-imag make-from-mag-ang)

  (install-polar-package)
  (install-rectangular-package)
  
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)))
