(module ordered-lists-set scheme
  (require "selection-sort.ss")
  (provide make-set element-of-set? adjoin-set intersection-set union-set)

  ;; simple and stupid selection sort
  (define (make-set . items)
    (selection-sort items
                    (lambda (x y)
                      (cond ((< x y) -1)
                            ((> x y)  1)
                            (else 0)))))

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