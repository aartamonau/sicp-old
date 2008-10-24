(module transforms scheme
  (require "tag-system/tag-system.ss"
           "scheme-numbers/scheme-numbers.ss" "rational-algebra/rational.ss"
           "complex-algebra/complex.ss"
           "coercion/coercion-table.ss"
           "operations.ss")
  (provide install-type-transforms table)

  (define (install-type-transforms)
    (put-coercion 'scheme-number 'rational scheme-number->rational)
    (put-coercion 'rational 'complex rational->complex)
    (put-coercion 'scheme-number 'complex scheme-number->complex)
    'done)

  (define (scheme-number->rational n)
    (make-rational (contents n) 1))

  (define (rational->complex r)
    (make-complex-from-real-imag (/ (numer r) (denom r))
                                 0))

  (define (scheme-number->complex n)
    (rational->complex
     (scheme-number->rational n))))
