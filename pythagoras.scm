(define square
  (lambda (x) (* x x)))

(define sum-squares
  (lambda (x y) (+ (square x) (square y))))

; Assuming sqrt is already defined
(define pythagoras
  (lambda (x y) (sqrt (sum-squares x y))))

(pythagoras 3 4)

