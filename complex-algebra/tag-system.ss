(module tag-system scheme
  (provide attach-tag type-tag contents)

  (define (attach-tag type-tag contents)
    (cons type-tag contents))

  (define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Incorrect data. There is no tags attached to it. [TYPE-TAG]" datum)))

  (define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Incorrect data. Can't fetch contents from the given data. [CONTENTS]" datum))))