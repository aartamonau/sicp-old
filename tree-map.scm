; SICP exercise 2.31

(define (tree-map func tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map func subtree)
	     (func subtree)))
       tree))