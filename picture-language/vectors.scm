; SICP ex. 2.46

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
		(xcor-vect b))
	     (+ (ycor-vect a)
		(ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
		(xcor-vect b))
	     (- (ycor-vect a)
		(ycor-vect b))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))
