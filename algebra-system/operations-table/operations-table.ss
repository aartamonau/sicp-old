;; This module is a simple imitation of data abstraction described
;; in SICP in 2.4
;; It will be thought as deprecated after full-featured one is created.

(module operations-table scheme
  (provide put get)

  (define table '())

  (define (put operation type implementation)
    (let ((updated (cons (cons (cons operation type)
                               implementation)
                         table)))
      (set! table updated)))

  (define (get operation type)
    (define pair (cons operation type))
    (define (iterate table)
      (cond ((null? table) null)
            ((equal? pair (caar table)) (cdar table))
            (else (iterate (cdr table)))))
    (iterate table)))
