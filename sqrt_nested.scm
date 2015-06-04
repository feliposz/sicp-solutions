(define (square x)
  (* x x))
(define (average a b)
  (/ (+ a b) 2))
(define (sqrt x)
    (define (improve guess)
      (average guess (/ x guess)))
    (define (good-enough? guess)
      (< (abs (- (square guess) x))
         .001))
    (define (try guess)
      (if (good-enough? guess)
          guess
          (try (improve guess))))
    (try 1))

(sqrt 2.0)
;1.414...
(sqrt 3.0)
;1.732...
(sqrt 4.0)
;2
