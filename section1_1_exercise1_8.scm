; Implementation of a cube root function using the same
; successive aproximation technique used for the square root

(define (cube x)
  (* x x x))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x))
     0.00001))

; Newton's method for aproximating a solution for a cubic root
(define (improve guess x)
  (/ 
   (+ (/ x (square guess)) 
      (* 2 guess))
   3))

(cbrt 27)
; 3
(cbrt 1000)
; 10
(cbrt 512)
; 8
(cbrt 1000000)
; 100
(cbrt 2)
; 1.259921...











