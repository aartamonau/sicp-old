;; SICP exercise 1.31
;; Higher order procudure for product in recursive form

(define (product term a b)
  (if (> a b)
      1
      (* (term a) (product term (+ a 1) b))))