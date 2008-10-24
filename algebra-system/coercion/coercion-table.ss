(module coercion-table scheme
  (provide put-coercion get-coercion table)

  (define table '())

  (define (put-coercion src-type dst-type implementation)
    (let ((updated (cons (cons (cons src-type dst-type)
                               implementation)
                         table)))
      (set! table updated)))

  (define (get-coercion src-type dst-type)
    (define pair (cons src-type dst-type))
    (define (iterate table)
      (cond ((null? table) null)
            ((equal? pair (caar table)) (cdar table))
            (else (iterate (cdr table)))))
    (if (equal? src-type dst-type)
        (lambda (x) x)
        (iterate table))))
