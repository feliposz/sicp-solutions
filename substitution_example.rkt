#lang scheme

; sum of squares
(define (sos x y)
  (+ (sq x) (sq y)))

; square
(define (sq x)
  (* x x))

; sum of the square of x and square of y
(sos 3 4)

;substitution process:
;(+ (sq 3) (sq 4))
;(+ (sq 3) (* 4 4))
;(+ (sq 3) 16)
;(+ (* 3 3) 16)
;(+ 9 16)
;25

; decrement x
(define (decr x)
  (- x 1))

; increment x
(define (incr x)
  (+ x 1))

; add x and y by iterating through add
(define (add x y)
  (if (= x 0)
      y
      (add (decr x) (incr y))))

(add 3 4)
;substitution process:
;(if (= 3 0) 4 (add (decr 3) (incr 4)))
;(add (decr 3) (incr 4))
;(add (decr 3) (+ 4 1))
;(add (decr 3) 5)
;(add (- 3 1) 5)
;(add 2 5)
;(if (= 2 0) 5 (add (decr 2) (incr 5)))
;(add (decr 2) (incr 5))
;(add (decr 2) (+ 5 1))
;(add (decr 2) 6)
;(add (- 2 1) 6)
;(add 1 6)
;(if (= 1 0) 6 (add (decr 1) (incr 6)))
;(add (decr 1) (+ 6 1))
;(add (decr 1) 7)
;(add (- 1 1) 7)
;(add 0 7)
;(if (= 0 0) 7 (add (decr 0) (incr 7)))
;7

; iteration:
; time  = O(x)
; space = O(1)

; add x and y by recursing into add-r
(define (add-r x y)
  (if (= x 0)
      y
      (incr (add-r (decr x) y))))

(add-r 3 4)
;(add-r 3 4)
;(if (= 3 0) 4 (incr (add-r (decr 3) 4)))
;(incr (add-r (decr 3) 4))
;(incr (add-r (- 3 1) 4))
;(incr (add-r 2 4))
;(incr (if (= 2 0) 4 (incr (add-r (decr 2) 4))))
;(incr (incr (add-r (decr 2) 4)))
;(incr (incr (add-r (- 2 1) 4)))
;(incr (incr (add-r 1 4)))
;(incr (incr (if (= 1 0) 4 (incr (add-r (decr 1) 4)))))
;(incr (incr (incr (add-r (decr 1) 4)))
;(incr (incr (incr (add-r (- 1 1) 4)))
;(incr (incr (incr (add-r 0 4)))
;(incr (incr (incr (if (= 0 0) 4 (incr (add-r (decr 0) 4)))))
;(incr (incr (incr 4)))
;(incr (incr (+ 4 1)))
;(incr (incr 5))
;(incr (+ 5 1))
;(incr 6)
;(+ 6 1)
;7


; recursion
; time  = O(x)
; space = O(x)

;calculating fibonacci numbers by recursion
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 4)
;tree of recursions for (fib 4)
;             fib4
;       fib3        fib2
;   fib2    fib1  fib1 fib0
;fib1 fib0   1     1    0
; 1    0

; time = O(fib n) -> number of calls grows in a "fibonaccian way"
; space = O(n) -> recursion depth

;fib0 -> t(1) s(1)
;fib1 -> t(1) s(1)
;fib2 -> t(3) s(2)
;fib3 -> t(5) s(3)
;fib4 -> t(9) s(4)
;fib5 -> t(15) s(5)
;fib6 -> t(25) s(6)
;fib7 -> t(41) s(7)

(fib 5)
;                          fib5
;             fib4                       fib3
;       fib3        fib2             fib2    fib1
;   fib2    fib1  fib1 fib0        fib1 fib0  1
;fib1 fib0   1     1    0           1    0
; 1    0

