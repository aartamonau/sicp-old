(module apply scheme
  (require "../tag-system/tag-system.ss" "../operations-table/operations-table.ss"
           "../coercion/coercion-table.ss")
  (provide apply-generic)

  (define (apply-generic op . args)
    (define (coerce items type)
      (if (null? items)
          '()
          (let* ((item (car items))
                 (coercion (get-coercion item
                                         type)))
            (if (not (null? coercion))
                (cons coercion
                      (coerce (cdr items) type))
                '(error)))))

   (define (correct-coercion? items)
     (not 
      (ormap (lambda (x) (equal? x 'error)) items)))

   (define (lookup-coercion items)
     (define (iter type before after)
       (let* ((c1 (coerce before type))
              (c2 (coerce after type)))
         (if (and (correct-coercion? c1)
                  (correct-coercion? c2))
             (append c1 (cons (get-coercion type type)
                              c2))
             (if (not (null? after))
                 (iter (car after)
                       (append before (list type))
                       (cdr after))
                 null))))
     (iter (car items) '() (cdr items)))

   (define (apply-coercion coercion items)
     (if (null? coercion)
         '()
         (cons ((car coercion) (car items))
               (apply-coercion (cdr coercion) (cdr items)))))

   (let* ((type-tags (map type-tag args))
          (proc (get op type-tags)))
     (if (not (null? proc))
         (apply proc (map contents args))
         (let ((coercion (lookup-coercion type-tags)))
           (if (not (null? coercion))
               (let* ((args (apply-coercion coercion args))
                      (proc (get op (map type-tag args))))
                 (if (not (null? proc))
                     (apply proc (map contents args))
                     (error "Operation for specified (inferred) types not found: "
                            (list op (map type-tag args)))))
               (error "Coercion for specified arguments not found: "
                      (list op type-tags))))))))
