; Recursive simplifier
(define (simplify expr var)
  (cond
    ; x
    ((atom? expr) expr)
    ; (+ a b)
    ((sum? expr)
      (simplify-sum (simplify (first-term expr) var)
                    (simplify (second-term expr) var)
                    var))
    ; (- a b)
    ((diff? expr)
      (simplify-diff (simplify (first-term expr) var)
                     (simplify (second-term expr) var)
                     var))
    ; (* a b)
    ((product? expr)
      (simplify-product (simplify (first-term expr) var)
                        (simplify (second-term expr) var)
                        var))
    ; (/ a b)
    ((fraction? expr)
      (simplify-fraction (simplify (first-term expr) var)
                         (simplify (second-term expr) var)
                         var))
    ; (^ a b)
    ((expnt? expr)
      (simplify-expnt (simplify (first-term expr) var)
                      (simplify (second-term expr) var)
                      var))
    ; If no rule matches, return an unchanged expression
    (else expr)))

; Simplification rules for '(+ a b)
(define (simplify-sum a b var)
  (cond
    ; (+ 1 2) -> 3
    ((and (number? a) (number? b)) (+ a b))
    ; (+ 0 b) -> b
    ((eq? a 0) b)
    ; (+ a 0) -> a
    ((eq? b 0) a)
    ; (+ a (- c a)) -> c
    ((and (pair? b) (diff? b) (eq? (second-term b) a))
      (first-term b))
    ; (+ (- c b) b) -> c
    ((and (pair? a) (diff? a) (eq? (second-term a) b))
      (first-term a))
    ; (+ a b) -> (+ a b)
    (else (make-sum a b))))

; Simplification rules for '(- a b)
(define (simplify-diff a b var)
  (cond 
    ; (- 1 2) -> -1
    ((and (number? a) (number? b)) (- a b))
    ; (- a 0) -> a
    ((eq? b 0) a)
    ; (- a a) -> 0
    ((eq? a b) 0)
    ; (- (+ b c) b) OR (- (+ c b) b) -> c
    ((and (pair? a) (sum? a) (eq? (first-term a) b))
      (second-term a))
    ((and (pair? a) (sum? a) (eq? (second-term a) b))
      (first-term a))
    ; (- a b) -> (- a b)
    (else (make-diff a b))))

; Rules for simplifying '(* a b)
(define (simplify-product a b var)
  (cond
    ; (* 1 2) -> 2
    ((and (number? a) (number? b)) (* a b))
    ; (* 0 b) -> 0
    ((eq? a 0) 0)
    ; (* a 0) -> 0
    ((eq? b 0) 0)
    ; (* 1 b) -> b
    ((eq? a 1) b)
    ; (* a 1) -> a
    ((eq? b 1) a)
    ; Associativity (if a is a number and b is a product)
    ; (* 2 (* 3 c)) OR (* 2 (* c 3)) -> (* 6 c)
    ; (* (* 3 c) 2) OR (* (* c 3) 2) -> (* 6 c)
    ((and (number? a) (pair? b) (product? b))
      (cond
        ((number? (first-term b))
          (make-product (* a (first-term b)) (second-term b)))
        ((number? (second-term b))
          (make-product (* a (second-term b)) (first-term b)))))
    ((and (number? b) (pair? a) (product? a))
      (cond
        ((number? (first-term a))
          (make-product (* b (first-term a)) (second-term a)))
        ((number? (second-term a))
          (make-product (* b (second-term a)) (first-term a)))))
    ; (* a b) -> (* a b)
    (else (make-product a b))))

; Rules for simplifying (/ a b)
(define (simplify-fraction a b var)
  (cond
    ; (/ 1 2) -> 0.5
    ((and (number? a) (number? b)) (/ a b))
    ; (/ 0 b) -> 0
    ((eq? a 0) 0)
    ; (/ a 1) -> a
    ((eq? b 1) a)
    ; (/ (* b c) b) OR (/ (* c b) b) -> c
    ((and (pair? a) (product? a) (atom? b))
      (cond
        ((eq? b (first-term a)) (second-term a))
        ((eq? b (second-term a)) (first-term a))))
    ; (/ a (* a c)) OR (/ a (* c a)) -> (/ 1 c)
    ((and (atom? a) (pair? b) (product? b))
      (cond
        ((eq? a (first-term b)) (make-fraction 1 (second-term b)))
        ((eq? a (second-term b)) (make-fraction 1 (first-term b)))))
    ; For (/ (* c d) (* e f))
    ((and (pair? a) (pair? b))
      (cond
        ((eq? (first-term a) (first-term b))
          (make-fraction (second-term a) (second-term b)))
        ((eq? (first-term a) (second-term b))
          (make-fraction (second-term a) (first-term b)))
        ((eq? (second-term a) (first-term b))
          (make-fraction (first-term a) (second-term b)))
        ((eq? (second-term a) (second-term b))
          (make-fraction (first-term a) (first-term b)))
        (else (make-fraction a b))))
    ; (/ a b) -> (/ a b)
    (else (make-fraction a b))))

; Rules for simplifying (^ a b)
(define (simplify-expnt a b var)
  (cond
    ; (^ 2 3) -> 8
    ((and (number? a) (number? b)) (exp (* b (log a))))
    ; (^ a 0) -> 1
    ((eq? b 0) 1)
    ; (^ a 1) -> a
    ((eq? b 1) a)
    ; (^ a b) -> (^ a b)
    (else (make-expnt a b))))

