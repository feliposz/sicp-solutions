; Solves a valid quadratic equation (i.e. delta >= 0)
; Returns the largest value for x

(define (quadratic-root a b c)
  (define delta
    (- (* b b) (* 4 a c)))
  (define x1
    (/ (+ (- b) (sqrt delta))
       (* 2 a)))
  (define x2
    (/ (- (- b) (sqrt delta))
       (* 2 a)))
  (if (> x1 x2)
      x1
      x2))

(quadratic-root 3 -7 2)
(quadratic-root -1 4 -4)
(quadratic-root 5 -6 5)
(quadratic-root 1 -4 3)
(quadratic-root 2 -6 -8)
(quadratic-root -1 0 4)
(quadratic-root 1 -6 8)
  
    