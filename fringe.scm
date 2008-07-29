; SICP exercise 2.28

(load "lists.scm")

(define (fringe items)
  (cond ((null? items) ())
	((not (pair? items)) (list items))
	(else (append (fringe (car items))
		      (fringe (cdr items))))))