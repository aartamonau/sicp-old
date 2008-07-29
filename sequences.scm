; SICP
; Some routines to work with sequences

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
	((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))