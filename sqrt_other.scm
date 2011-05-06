(define sqrt
  (lambda (x) (sqrt-loop 1.0 x)))

(define sqrt-loop
  (lambda (guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-loop (improve guess x) x))))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(sqrt 2)



