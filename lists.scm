;; SICP
;; Operations with lists.

(define (list-ref items n)
  (if (= n 0)
      (car iterms)
      (list-ref (cdr items (- n 1)))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (iter a count)
    (if (null? a)
	count
	(iter (cdr a) (+ 1 count))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (if (null? items)
      (error "List must not be empty")
      (if (null? (cdr items))
	  items
	  (last-pair (cdr items)))))

(define (reverse items)
    (if (null? items)
	(list)
	(append (reverse (cdr items))
		 (list (car items)))))