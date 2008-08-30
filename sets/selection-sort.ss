(module selection-sort scheme
  (provide selection-sort)

  (define (foldl-wrapper fun items)
    (if (null? items)
        (error "List must contain at least one item")
        (foldl fun (car items) (cdr items))))

  ;; places "x" element to the first place in the list
  ;; other elements in the list don't preserve their order
  (define (rearrange-list x items cmp)
    (define (helper head tail)
      (cond ((null? tail)
             (error "There is no such element in the list"))
            ((= 0 (cmp x (car tail)))
             (cons x (append head (cdr tail))))
            (else (helper (cons (car tail) head) (cdr tail)))))
    (helper '() items))

  (define (selection-sort items cmp)
    (define (helper sorted unsorted)
      (if (null? unsorted)
          (reverse sorted)
          (let ((rearranged (rearrange-list (foldl-wrapper (lambda (x y) (if (< (cmp x y) 0)
                                                                             x
                                                                             y))
                                                           unsorted)
                                            unsorted
                                            cmp)))
            (helper (cons (car rearranged) sorted) (cdr rearranged)))))
    (helper '() items)))

  