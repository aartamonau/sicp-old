(module transforms scheme
  (require "tag-system/tag-system.ss"
           "scheme-numbers/scheme-numbers.ss" "rational-algebra/rational.ss"
           "complex-algebra/complex.ss"
           "coercion/coercion-table.ss"
           "operations.ss")
  (provide install-type-transforms table)

  (define (install-type-transforms)
    (put-coercion 'rational 'scheme-number rational->scheme-number)
    (put-coercion 'scheme-number 'complex scheme-number->complex)
    (put-coercion 'rational 'complex rational->complex)
    'done)

  (define (rational->scheme-number r)
    (make-scheme-number (/ (numer r) (denom r))))

  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n)))

  (define (rational->complex r)
    (scheme-number->complex
     (rational->scheme-number r))))
