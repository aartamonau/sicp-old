(module higher-order-operations scheme
  (provide square-of-four split)

  (define (square-of-four tl tr bl br)
    (lambda (painter)
      (let ((top (beside (tl painter) (tr painter)))
            (bottom (beside (bl painter) (br painter))))
        (below bottom top))))

  (define (split outer-splitter inner-splitter)
    (define (split-helper painter n)
      (if (= n 0)
          painter
          (let ((smaller (split-helper painter (- n 1))))
            (outer-splitter painter
                            (inner-splitter smaller smaller)))))
    split-helper))
