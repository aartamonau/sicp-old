(module ordered-lists-set scheme
  (provide make-set element-of-set? adjoin-set intersection-set union-set)

  (define (min-list items)
    (define (helper current-min items)
      (cond ((null? items) current-min)
            ((< (car items) current-min) (helper (car items) (cdr items)))
            (else (helper current-min (cdr items)))))
    (if (null? items)
        (error "List must contain at least one item")
        (helper (car items) (cdr items))))

  ;; places "x" element to the first place in the list
  ;; other elements in the list don't preserve their order
  (define (rearrange-list x items)
    (define (helper head tail)
      (cond ((null? tail)
             (error "There is no such element in the list"))
            ((equal? x (car tail))
             (cons x (append head (cdr tail))))
            (else (helper (cons (car tail) head) (cdr tail)))))
    (helper '() items))

  (define (sort-list items)
    (define (helper sorted unsorted)
      (if (null? unsorted)
          (reverse sorted)
          (let ((rearranged (rearrange-list (min-list unsorted) unsorted)))
            (helper (cons (car rearranged) sorted) (cdr rearranged)))))
    (helper '() items))

  ;; simple and stupid selection sort
  (define (make-set . items)
    (sort-list items))

  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set)) false)
          (else (element-of-set? x (cdr set)))))

  (define (adjoin-set x set)
    (cond ((null? set) (make-set x))
          ((= x (car set)) set)
          ((< x (car set)) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))))

  (define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x (car set1)) (y (car set2)))
          (cond ((= x y) (cons x (intersection-set (cdr set1) (cdr set2))))
                ((< x y) (intersection-set (cdr set1) set2))
                ((> x y) (intersection-set set1 (cdr set2)))))))

  (define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((x (car set1)) (y (car set2)))
                  (cond ((= x y) (cons x (union-set (cdr set1) (cdr set2))))
                        ((< x y) (cons x (union-set (cdr set1) set2)))
                        (else (cons y (union-set set1 (cdr set2))))))))))