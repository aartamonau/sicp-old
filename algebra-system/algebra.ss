(module algebra scheme
  (require "complex-algebra/complex.ss" "rational-algebra/rational.ss" "integer-algebra/integer-algebra.ss" "generic/apply.ss"
           "operations-table/operations-table.ss"
           "operations.ss" "transforms.ss")
  (provide make-complex-from-real-imag make-complex-from-mag-ang make-rational make-integer
           add sub mul div equ? =zero? numer denom table get)

  (install-type-operations))

