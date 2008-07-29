;; SICP exercise 1.29
;; Integral calculation using Simpson method.

(define (simpson-method f a b n)
  (define steps (if (odd? n)
		    (+ n 1)
		    n))
  (define h (/ (- b a) steps))
  (define (item k)
    (cond ((or (= k 0) (= k steps)) (f (+ a (* h k))))
	  ((odd? k) (* 4 (f (+ a (* h k)))))
	  (else (* 2 (f (+ a (* h k)))))))
  (define (sum-iter current-sum k)
    (if (and (>= k 0) (<= k steps))
	(sum-iter (+ current-sum (item k)) (+ k 1))
	current-sum))
  (* (/ h 3) (sum-iter 0.0 0)))