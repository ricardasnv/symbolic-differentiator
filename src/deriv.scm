; Recursive symbolic differentiator
(define (deriv expression variable)
  (define simplified-expression (simplify expression variable))
  (define (deriv-iter expr var)
    (cond
      ((same-var? expr var) 1)
      ((constant? expr var) 0)
      ; Addition rule
      ((sum? expr)
        (make-sum (deriv-iter (first-term expr) var)
                  (deriv-iter (second-term expr) var)))
      ; Subtraction rule
      ((diff? expr)
        (make-diff (deriv-iter (first-term expr) var)
                   (deriv-iter (second-term expr) var)))
      ; Product rule
      ((product? expr)
        (make-sum (make-product (first-term expr) (deriv-iter (second-term expr) var))
                  (make-product (second-term expr) (deriv-iter (first-term expr) var))))
      ; Fraction rule
      ((fraction? expr)
        (make-fraction (make-diff (make-product (deriv-iter (first-term expr) var)
                                                (second-term expr))
                                  (make-product (first-term expr)
                                                (deriv-iter (second-term expr) var)))
                       (make-product (second-term expr)
                                     (second-term expr))))
      ; Power rule (only for constant exponents)
      ((and (expnt? expr) (constant? (second-term expr) var))
        (make-product
          (make-product (second-term expr)
                        (make-expnt (first-term expr)
                                    (make-diff (second-term expr) '1)))
          (deriv-iter (first-term expr) var)))
      ; If no rule matches, complain
      (else (error "deriv" "Bad expression" expr))))

  ; Compute the derivative, simplify and return
  (simplify (deriv-iter simplified-expression variable) variable))

