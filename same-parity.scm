; SICP exercise 2.20

(load "lists.scm")

(define (same-parity first . items)
  (define (iter items result func)
    (if (null? items)
	result
	(iter (cdr items)
	      ((lambda () (if (func (car items))
			      (cons (car items) result)
			      result)))
	      func)))
  (reverse   (iter items
		   (list first)
		   ((lambda () (if (odd? first)
				   odd?
				   even?))))))