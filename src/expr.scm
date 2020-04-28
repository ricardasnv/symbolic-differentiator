; Structure of an expression: (<operator> <expr1> <expr2>)
(define operator car)
(define first-term cadr)
(define second-term caddr)

; Recognized operators
(define add-operator '+) ; (+ a b) -> a+b
(define sub-operator '-) ; (- a b) -> a-b
(define mul-operator '*) ; (* a b) -> a*b
(define div-operator '/) ; (/ a b) -> a/b
(define pow-operator '^) ; (^ a b) -> a^b

; Shorthands for determining expression type
(define (sum? expr) (eq? (operator expr) add-operator))
(define (diff? expr) (eq? (operator expr) sub-operator))
(define (product? expr) (eq? (operator expr) mul-operator))
(define (fraction? expr) (eq? (operator expr) div-operator))
(define (expnt? expr) (eq? (operator expr) pow-operator))

(define (same-var? expr var)
  (and (atom? expr) (eq? expr var)))

; CDR down the expression and return true if it doesn't contain the free variable
(define (constant? expr var)
  (cond
    ((atom? expr) (if (eq? expr var) #f #t))
    (else (and (constant? (first-term expr) var)
               (constant? (second-term expr) var)))))

(define (atom? expr)
  (not (pair? expr)))

