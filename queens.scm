; SICP ex. 2.42

(load "sequences.scm")
(load "flatmap.scm")
(load "enumerate-interval.scm")

(define (queens board-size)
  (define empty-board ())
  

  (define (adjoin-position new-row k rest-of-queens)
    (cons (list k new-row) rest-of-queens))
  

  (define (safe? k positions)
    (define (get-k-pair k positions)
      (if (= k (caar positions))
	  (car positions)
	  (get-k-pair k (cdr positions))))

    (define (remove-k k positions)
      (if (= k (caar positions))
	  (cdr positions)
	  (cons (car positions)
		(remove-k k (cdr positions)))))

    (let ((new-row (cadr (get-k-pair k positions))))
      (accumulate (lambda (x y) (and x y))
		  #t
		  (map (lambda (pair)
			 (let ((column (car pair))
			       (row (cadr pair)))
			   (not (or (= row new-row)
				    (= new-row (+ row (- k column)))
				    (= new-row (- row (- k column)))))))
		       (remove-k k positions)))))


  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))

  (queen-cols board-size))