;; SICP exercise 1.43
;; Repeated applying of function


(load "compose.scm")

(define (repeated f times)
  (define (iter f times residue)
    (cond ((= times 0) residue)
	  ((even? times) (iter (compose f f)
			       (/ times 2)
			       residue))
	  (else (iter f
		      (- times 1)
		      (compose residue f)))))
  (iter f times (lambda (x) x)))