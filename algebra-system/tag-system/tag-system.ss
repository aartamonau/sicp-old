(module tag-system scheme
  (provide attach-tag type-tag contents)

  (define (attach-tag type-tag contents)
    (if (eqv? type-tag 'scheme-number)
        contents
        (cons type-tag contents)))

  (define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((pair? datum) (car datum))
          (else
           (error "Incorrect data. There is no tags attached to it. [TYPE-TAG]" datum))))

  (define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else
           (error "Incorrect data. Can't fetch contents from the given data. [CONTENTS]" datum)))))
