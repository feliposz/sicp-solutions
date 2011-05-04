#lang racket
3
17.4
5
(+ 3 17.4 5)
(+ 3 (* 5 6) 8 2)
(+ (* 3 5)
   (* 47
      (- 20 6.8))
   12)
(define a (* 5 5))
(* a a)
(define b (+ a (* 5 a)))
b
(* a (/ b 5))
(+ a (/ b 5))
(define (square x)
  (* x x))
(square 5)
(define square-lambda
  (lambda (x)
    (* x x)))
(square-lambda 12)
square
(+ (square 3) (square 4))
(square (square (square 1001)))
square
(define (average x y)
  (/ (+ x y) 2))
(define (mean-square x y)
  (average (square x) (square y)))
(mean-square 3 4)
(mean-square 2 3)
+
=
(- 5)
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(abs -5)
