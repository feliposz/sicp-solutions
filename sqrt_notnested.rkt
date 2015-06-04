#lang racket
(define (square x)
    (* x x))

(define (average x y)
    (/ (+ x y) 2))

(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (try guess x)
    (if (good-enough? guess x)
        guess
        (try (improve guess x) x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x))
       .001))

(define (sqrt x)
    (try 1 x))

