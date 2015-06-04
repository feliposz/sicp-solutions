(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define a (cons 1 2))
(define b (cons a a))
(define c (cons 1 b))
(define d (cons b b))
(define e (cons 1 2))
(set-cdr! e e)

a
b
c
d
;e

(count-pairs a) ; 1 pair
(count-pairs b) ; 3 pairs (actually, there are just 2)
(count-pairs c) ; 4 pairs (actually, there are just 3)
(count-pairs d) ; 7 pairs (actyally, there are just 4)
;(count-pairs e)
;e is a circular self-referencing structure
;count-pairs never returns
