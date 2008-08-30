(module binary-tree scheme
  (require "selection-sort.ss")
  (provide list->tree ordered-list->tree tree->ordered-list
           left-branch right-branch entry
           element-of-tree? insert-tree
           list->tree)

  (define (make-tree-node entry left right)
    (list entry left right))

  (define (entry tree) (car tree))

  (define (left-branch tree) (cadr tree))

  (define (right-branch tree) (caddr tree))

  (define (element-of-tree? x tree)
    (if (null? tree)
        false
        (let ((y (entry tree)))
          (cond ((= x y) true)
                ((< x y) (element-of-tree? x (left-branch tree)))
                (else (element-of-tree? x (right-branch tree)))))))

  (define (insert-tree x tree)
    (cond ((null? tree) (make-tree-node x '() '()))
          ((= x (entry tree)) tree)
          ((< x (entry tree))
           (make-tree-node (entry tree)
                           (insert-tree x (left-branch tree))
                           (right-branch tree)))
          ((> x (entry tree))
           (make-tree-node (entry tree)
                           (left-branch tree)
                           (insert-tree x (right-branch tree))))))

  ;; ineffective
  (define (list->tree items)
    (ordered-list->tree
     (selection-sort items
                     (lambda (x y)
                       (cond ((< x y) -1)
                             ((> x y)  1)
                             (else     0))))))

  (define (ordered-list->tree elements)
    (car (partial-tree elements (length elements))))

  (define (tree->ordered-list tree)
    (define (copy-to-list tree result-list)
      (if (null? tree)
          result-list
          (copy-to-list (left-branch tree)
                        (cons (entry tree)
                              (copy-to-list (right-branch tree)
                                            result-list)))))
    (copy-to-list tree '()))

  (define (partial-tree elements n)
    (if (= n 0)
        (cons '() elements)
        (let* ((left-size (quotient (- n 1) 2))
               (left-result (partial-tree elements left-size))
               (left-tree (car left-result))
               (non-left-elements (cdr left-result))
               (right-size (- n (+ left-size 1)))
               (this-entry (car non-left-elements))
               (right-result (partial-tree (cdr non-left-elements)
                                           right-size))
               (right-tree (car right-result))
               (remaining-elements (cdr right-result)))
          (cons (make-tree-node this-entry left-tree right-tree)
                remaining-elements)))))