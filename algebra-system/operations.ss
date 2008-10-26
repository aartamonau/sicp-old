(module operations scheme
  (require "complex-algebra/complex.ss" "rational-algebra/rational.ss" "integer-algebra/integer-algebra.ss" "generic/apply.ss"
           "operations-table/operations-table.ss")
  (provide make-complex-from-real-imag make-complex-from-mag-ang make-rational make-integer
           add sub mul div equ? =zero? numer denom project real-part imag-part magnitude angle)

  (install-complex-package)
  (install-rational-package)
  (install-integer-algebra-package)

  (define (add x y) (apply-generic 'add x y))
  (define (sub x y) (apply-generic 'sub x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (div x y) (apply-generic 'div x y))
  (define (equ? x y) (apply-generic 'equ? x y))
  (define (=zero? x) (apply-generic '=zero? x))
  (define (numer x) (apply-generic 'numer x))
  (define (denom x) (apply-generic 'denom x))
  (define (project x) (apply-generic 'project x))
  (define (real-part x) (apply-generic 'real-part x))
  (define (imag-part x) (apply-generic 'imag-part x))
  (define (magnitude x) (apply-generic 'magnitude x))
  (define (angle x) (apply-generic 'angle x))

  (define (make-integer n)
    ((get 'make 'integer) n))

  (define (make-rational n d)
    ((get 'make 'rational) n d))

  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

  (define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)))