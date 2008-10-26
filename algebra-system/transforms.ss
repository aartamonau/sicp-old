(module transforms scheme
  (require "tag-system/tag-system.ss"
           "integer-algebra/integer-algebra.ss" "rational-algebra/rational.ss"
           "complex-algebra/complex.ss"
           "coercion/coercion-table.ss"
           "operations.ss")
  (provide install-type-operations table) 

  (define (install-type-operations)
    (put-coercion 'drop '(complex) complex->rational)
    (put-coercion 'drop '(rational) rational->integer)
    (put-coercion 'drop '(integer) (lambda (x) #f))
    (put-coercion 'raise '(integer) integer->rational)
    (put-coercion 'raise '(rational) rational->complex)
    (put-coercion 'raise '(complex) (lambda (x) #f))

    (set-rank-function! type-rank)

    'done)

  (define (type-rank type)
    (cond ((eq? type 'integer) 0)
          ((eq? type 'rational) 1)
          ((eq? type 'complex) 2)
          (else (error "[TYPE-RANK] Not registered type"))))

  (define (integer->rational n)
    (make-rational (contents n) 1))

  (define (rational->complex r)
    (make-complex-from-real-imag (/ (numer r) (denom r)) 0))

  (define (complex->rational z)
    (let ((projection (project z)))
      (if (equ? projection z)
          (make-rational (numerator (real-part projection))
                         (denominator (real-part projection)))
          #f)))

  (define (rational->integer r)
    (let ((projection (project r)))
      (if (equ? projection r)
          (make-integer (numer projection))
          #f))))
