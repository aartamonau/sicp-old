(module basic-expressions scheme
  (require "variable.ss")
  (provide make-sum sum? addend augend
           make-product product? multiplier multiplicand
           make-exponentiation exponentiation? base exponent
           deriv)

  (define (=number? exp num)
    (and (number? exp)
         (= exp num)))
  (define (<number? exp num)
    (and (number? exp)
         (< exp num)))
  (define (<=number? exp num)
    (and (number? exp)
         (<= exp num)))

  (define (passes-sanity-test? items)
      (eq? (memf (lambda (x) (not (or (number? x)
                                      (symbol? x)
                                      (sum? x)
                                      (product? x)
                                      (exponentiation? x))))
                 items)
           #f))

  (define (split-list matches? items)
    (foldl (lambda (item current-result)
             (let ((true-items (car current-result))
                   (false-items (cadr current-result)))
               (if (matches? item)
                   (list (cons item true-items)
                         false-items)
                   (list true-items
                         (cons item false-items)))))
           (list '() '())
           items))

  ;; collects all identical elements into one pair (element, count)
  (define (collect-items identical? less-than? items)
    (define (helper previous count current-result sorted-items)
      (if (null? sorted-items)
          (cons (cons previous count) current-result)
          (let ((head (car sorted-items))
                (tail (cdr sorted-items)))
            (if (identical? previous head)
                (helper previous (+ count 1) current-result tail)
                (helper head 1 (cons (cons previous count) current-result) tail)))))
    (if (null? items)
        '()
        (let ((sorted (sort items less-than?)))
          (helper (car sorted) 1 '() (cdr sorted)))))

  (define (expr->string expr)
    (cond ((null? expr) "")
          ((symbol? expr) (symbol->string expr))
          ((number? expr) (number->string expr))
          ((list? expr) (string-append "("
                                       (substring (foldl (lambda (x prefix)
                                                           (string-append prefix " " (expr->string x)))
                                                         ""
                                                         expr)
                                                  1)
                                       ")"))
          (else (error "Invalid type of expression. [expr->string]"))))

  (define (linguistic-expr=? x y)
    (string=? (expr->string x)
              (expr->string y)))

  (define (linguistic-expr<=? x y)
    (string<=? (expr->string x)
               (expr->string y)))

  ;; collects numbers and expressions if possible
  ;; in the simpliest case comparison operations define only symblo-to-symbol comparisons
  ;; in more complex case those can analize the structure of expressions

  ;; nowadays function opens suboperations of the same type so associativeness is needed
  (define (commutative-associative-operation-collect numbers-collector initial-number
                                                     expr-equal? expr-less-than? expr-transform
                                                     operation-symbol)
    (define (collector items)
      (if (passes-sanity-test? items)
          (let* ((splited (split-list number? items))
                 (numbers (car splited))
                 (expressions (cadr splited))
                 (folded-numbers (foldl numbers-collector initial-number numbers))
                 (collected-expressions (collect-items expr-equal? expr-less-than? expressions))
                 (post-processed-expressions (map (lambda (item)
                                                    (let ((expression (car item))
                                                          (count (cdr item)))
                                                      (expr-transform expression count)))
                                                  collected-expressions)))
            ;; hack
            ;; always list is returned. even if it contains only one element
            ;; this is used in helper procedure below
            (cond ((null? post-processed-expressions) (list folded-numbers))
                  ((and (= folded-numbers initial-number)
                        (not (= (length post-processed-expressions) 1)))
                   (cons operation-symbol post-processed-expressions))
                  ((and (= folded-numbers initial-number)
                        (= (length post-processed-expressions) 1))
                   post-processed-expressions)
                  (else (cons operation-symbol (cons folded-numbers post-processed-expressions)))))
          (error "Unknown element found. [commutative-operation-collect]")))
    (lambda (items)
      (define (helper items previous)
        (let ((result (collector items)))
          (cond ((= (length result) 1) (car result)) ; hack branch. see above
                ((linguistic-expr<=? result previous) result)
                (else (helper (cdr result) result)))))
      (helper items '() )))
                
        

  (define (make-sum-from-list items)
    ((commutative-associative-operation-collect + 0
                                                linguistic-expr=?
                                                linguistic-expr<=?
                                                make-product
                                                '+)
     items))

  (define (make-sum x y . rest)
    (make-sum-from-list (cons x (cons y rest))))

  (define (sum? x)
    (and (pair? x)
         (eq? (car x) '+)))
  
  (define (addend s) (cadr s))
  (define (augend s)
    (let ((rest (cddr s)))
      (if (> (length rest) 1)
          (cons '+ rest)
          (car rest))))

  (define (make-product-from-list items)
    (let ((pre-result ((commutative-associative-operation-collect * 1
                                                                  linguistic-expr=?
                                                                  linguistic-expr<=?
                                                                  make-exponentiation
                                                                  '*)
                       items)))
      (if (or (not (list? pre-result))
              (and (list? pre-result)
                   (eq? (memf (lambda (x)
                                (eq? x 0))
                              (cdr pre-result))
                        #f)))
          pre-result
          0)))

  (define (make-product x y . rest)
    (make-product-from-list (cons x (cons y rest))))
  (define (product? x)
    (and (pair? x)
         (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p)
    (let ((rest (cddr p)))
      (if (> (length rest) 1)
          (cons '* rest)
          (car rest))))

  (define (make-exponentiation b e)
    (cond ((and (=number? b 0) (<=number? e 0) (error "Undefined value. (0 ^ (<=0))")))
          ((=number? b 0) 0)
          ((=number? b 1) 1)
          ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e) (expt b e)))
          (else (list '^ b e))))

  (define (make-exponentiation-from-list items)
    (if (= (length items) 2)
        (make-exponentiation (car items) (cadr items))
        (error "Operation needs two arguments exactly. [make-exponention-from-list]")))

  (define (exponentiation? x) (eq? (car x) '^))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))

  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum (make-product (deriv (multiplier exp) var)
                                   (multiplicand exp))
                     (make-product (deriv (multiplicand exp) var)
                                   (multiplier exp))))
          ((exponentiation? exp)
           (let ((base (base exp))
                 (exponent (exponent exp)))
             (make-product exponent
                           (make-product (make-exponentiation base (make-sum exponent -1))
                                         (deriv base var)))))
          
          (else
           (error "Unknown expression type -- DERIV" exp))))


  (define (make-expression-prefix quoted-expression)
    (define (helper expression 
    (define (helper epxression top-level-operation)
      (map (lambda (x)
             (if (list? x)
                 (cond ((null? x) (error "NULL expression. [make-expression-prefix]"))
                       ((sum? x) (make-sum-from-list (cdr x)))
                       ((product? x) (make-product-from-list (cdr x)))
                       ((exponentiation? x) (make-exponentiation-from-list))
                       (else (error (string-append "(" (list->string x) ") "
                                                   "Unknown expression. [make-expression-prefix]"))))
                 
                                                   
