(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


(sine 12.15)
; a) Procedure p is executed 4 times for 12.15
; b) Resource requirements for procedure
; space = O(log n)
; time = O(log n)
;
; The number of times sine is called is dependent on value of angle
; angle is divided by 3 every recursion, reducing the "size" of the
; problem to 1/3


