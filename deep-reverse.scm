; SICP exercise 2.27

(load "lists.scm")
(load "for-each.scm")

(define (deep-reverse items)
  (cond ((pair? items) (map deep-reverse (reverse items)))
	(else items)))
  