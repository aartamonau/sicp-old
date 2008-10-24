(module algebra scheme
  (require "complex-algebra/complex.ss" "rational-algebra/rational.ss" "scheme-numbers/scheme-numbers.ss" "generic/apply.ss"
           "operations-table/operations-table.ss"
           "operations.ss" "transforms.ss")
  (provide make-complex-from-real-imag make-complex-from-mag-ang make-rational make-scheme-number
           add sub mul div equ? =zero? numer denom table)

  (install-type-transforms))
