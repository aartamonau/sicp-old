;; SICP example.
;; Chapter 2.1.1
;; Rational numbers' computation system.
;; Changed in conforming to exercise 2.1

(load "gcd.scm")

(define (sign x)
  (cond ((= x 0) 0)
	((> x 0) 1)
	(else -1)))

(define (make-rat n d)
  (let ((g (gcd n d))
	(frac-sign (* (sign n) (sign d))))
    (cons (* (abs (/ n g)) frac-sign)
	  (abs (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (if (not (= 0 (numer x)))
      (begin (display "/")
	     (display (denom x)))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))