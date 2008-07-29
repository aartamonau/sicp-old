; SICP exercise 2.23

(define (for-each func items)
  (cond ((null? items) #t)
	(else (func (car items))
	      (for-each func (cdr items)))))
