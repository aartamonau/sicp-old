(module binary-tree-set scheme
  (require "binary-tree.ss")
  (provide make-set set->list adjoin-set
           element-of-set?
           union-set intersection-set)

  (define (make-set . items)
    (list->tree items))

  (define (set->list set)
    (tree->ordered-list set))

  (define (adjoin-set x set)
    (insert-tree x set))

  (define (element-of-set? x set)
    (element-of-tree? x set))

  ;; it's analogue of intersection operation from
  ;; ordered-lists-set.ss
  (define (intersect-sorted-lists items1 items2)
    (if (or (null? items1) (null? items2))
        (make-set '())        
        (let ((x (car items1))
              (y (car items2)))
          (cond ((= x y) (cons x (intersect-sorted-lists (cdr items1) (cdr items2))))
                ((< x y) (intersect-sorted-lists (cdr items1) items2))
                (else (intersect-sorted-lists items1 (cdr items2)))))))

  ;; yet another analogue
  (define (union-ordered-lists items1 items2)
    (cond ((null? items1) items2)
          ((null? items2) items1)
          (else
           (let ((x (car items1))
                 (y (car items2)))
             (cond ((= x y) (cons x (union-ordered-lists (cdr items1) (cdr items2))))
                   ((< x y) (cons x (union-ordered-lists (cdr items1) items2)))
                   (else (cons y (union-ordered-lists items2 (cdr items1)))))))))

  (define (union-set set1 set2)
    (ordered-list->tree (union-ordered-lists
                         (tree->ordered-list set1)
                         (tree->ordered-list set2))))

  (define (intersection-set set1 set2)
    (ordered-list->tree (intersect-sorted-lists
                         (tree->ordered-list set1)
                         (tree->ordered-list set2)))))
