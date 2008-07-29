;; SICP ex. 2.5
;; Yet another one cons representation.

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car c)
  (if (= (remainder c 2) 0)
      (+ 1 (car (/ c 2)))
      0))

(define (cdr c)
  (if (= (remainder c 3) 0)
      (+ 1 (cdr (/ c 3)))
      0))