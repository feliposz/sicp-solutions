; A test implementation of cons, car and cdr in terms of procedures

(define (mycons x y)
  (lambda (m) (m x y)))

(define (mycar z)
  (z (lambda (p q) p)))

(define (mycdr z)
  (z (lambda (p q) q)))

(define mypair (mycons 1 2))
;(define mypair (lambda (m) (m 1 2)))

(mycar mypair)
;(mypair (lambda (p q) p))
;((lambda (p q) p) 1 2)
;1

(mycdr mypair)
;(mypair (lambda (p q) q))
;((lambda (p q) q) 1 2)
;2
