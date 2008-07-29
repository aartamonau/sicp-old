;; SICP ex. 2.6
;; Numbers' representation using Church numerals.

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add m n)
  (lambda (f) (lambda (x) (m f ((n f) x)))))

(define (church-to-num n)
  (define (inc x) (+ x 1))
  ((n inc) 0))