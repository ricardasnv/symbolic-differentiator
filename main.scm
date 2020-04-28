; Entry point of program
(module main
  (include "src/deriv.scm")
  (include "src/expr.scm")
  (include "src/simplify-expr.scm")
  (include "src/make-expr.scm"))

(define test-expr-1 '(^ (* a x) (+ a b))) ; (ax)^(a+b)
(define test-expr-2 '(/ (* a x) (* b x))) ; (ax/bx)
(define test-expr-3 '(+ (+ 1 2) (+ (- a x) x))) ; ((1+2)+((a-x)+x))
(define test-expr-4 '(+ (+ (* a (^ x 2)) (* b x)) c)) ; ((ax^2+bx+c))
(define test-expr-5 '(* 2 (* 3 (* x (* 4 5))))) ; (2*(3*(x*(4*5))))
(define test-expr-6 '(/ a (^ y (/ 1 2)))) ; (a/y^(1/2))

(printf "Derivative of ~a ---> ~a\n" test-expr-1 (deriv test-expr-1 'x))
(printf "Derivative of ~a ---> ~a\n" test-expr-2 (deriv test-expr-2 'x))
(printf "Derivative of ~a ---> ~a\n" test-expr-3 (deriv test-expr-3 'x))
(printf "Derivative of ~a ---> ~a\n" test-expr-4 (deriv test-expr-4 'x))
(printf "Derivative of ~a ---> ~a\n" test-expr-5 (deriv test-expr-5 'x))
(printf "Derivative of ~a ---> ~a\n" test-expr-6 (deriv test-expr-6 'y))

