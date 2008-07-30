;; SICP ex. 2.54

(define (list-equal? x y)
  (cond ((or (symbol? x) (symbol? y)) (eq? x y))
        ((and (eq? x '()) (eq? y '())) #t)
        (else (and (list-equal? (car x) (car y))
                   (list-equal? (cdr x) (cdr y))))))
         