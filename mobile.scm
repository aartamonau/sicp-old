; SICP exercise 2.29
; Fucking shit. I spent to much time doing it.

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (weight? branch)
  (not (pair? branch)))

(define (total-weight structure)
  (cond ((weight? structure) structure)
	(else (+ (total-weight (branch-structure (left-branch structure)))
		 (total-weight (branch-structure (right-branch structure)))))))

(define (balanced? mobile)
  (cond ((weight? mobile) #t)
	(else (and (balanced? (branch-structure (left-branch mobile)))
		   (balanced? (branch-structure (right-branch mobile)))
		   (= (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
		      (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile)))))))))