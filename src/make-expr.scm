(define (make-sum a b)
  (list add-operator a b))

(define (make-diff a b)
  (list sub-operator a b))

(define (make-product a b)
  (list mul-operator a b))

(define (make-fraction a b)
  (list div-operator a b))

(define (make-expnt a b)
  (list pow-operator a b))

