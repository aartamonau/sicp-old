(module operations scheme
  (require "complex-algebra/complex.ss" "rational-algebra/rational.ss" "scheme-numbers/scheme-numbers.ss" "generic/apply.ss"
           "operations-table/operations-table.ss")
  (provide make-complex-from-real-imag make-complex-from-mag-ang make-rational make-scheme-number
           add sub mul div equ? =zero? numer denom)

  (install-complex-package)
  (install-rational-package)
  (install-scheme-number-package)

  (define (add x y) (apply-generic 'add x y))
  (define (sub x y) (apply-generic 'sub x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (div x y) (apply-generic 'div x y))
  (define (equ? x y) (apply-generic 'equ? x y))
  (define (=zero? x) (apply-generic '=zero? x))
  (define (numer x) (apply-generic 'numer x))
  (define (denom x) (apply-generic 'denom x))

  (define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

  (define (make-rational n d)
    ((get 'make 'rational) n d))

  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

  (define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)))