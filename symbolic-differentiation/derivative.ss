(module derivative scheme
  (require "variable.ss" "basic-expressions.ss")
  (provide deriv)

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
          ((exponentiatioin? exp)
           (let ((base (base exp))
                 (exponent (exponent exp)))
             (make-product exponent
                           (make-product (make-exponentiation base (make-sum exponent -1))
                                         (deriv base var)))))
             
          (else
           (error "Unknown expression type -- DERIV" exp)))))
                                                      