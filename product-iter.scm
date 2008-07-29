;; SICP exercise 1.31
;; Higher order procudure for product in iterative form

(define (product term a b)
  (define (product-iter a curr-product)
    (if (> a b)
	curr-product
	(product-iter (+ 1 a) (* (term a)
				  curr-product))))
  (product-iter a 1))