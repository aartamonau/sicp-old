(module coercion-table scheme
  (require "../tag-system/tag-system.ss")
  (provide put-coercion set-rank-function! coerce-var rank table) 

  (define table '())
  (define rank '())

  (define (raise x) ((get-coercion 'raise (list (type-tag x))) x))
  (define (drop x) ((get-coercion 'drop (list (type-tag x))) x))

  (define (set-rank-function! f)
    (set! rank f))

  (define (coerce-var src dst)
    (let* ((src-type (type-tag src))
           (dst-type (type-tag dst))
           (src-rank (rank src-type))
           (dst-rank (rank dst-type)))
      (cond ((= src-rank dst-rank) src)
            ((< src-rank dst-rank) (coerce-var (raise src) dst))
            (else (let ((dropped-var (drop src)))
                    (if dropped-var
                        (coerce-var dropped-var dst)
                        '()))))))

  (define (put-coercion operation type implementation)
    (let ((updated (cons (cons (cons operation type)
                               implementation)
                         table)))
      (set! table updated)))

  (define (get-coercion operation type)
    (define pair (cons operation type))
    (define (iterate table)
      (cond ((null? table) null)
            ((equal? pair (caar table)) (cdar table))
            (else (iterate (cdr table)))))
    (iterate table)))
