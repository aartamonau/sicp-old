; SICP chapter 2.2.3

(load "sequences.scm")

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
  