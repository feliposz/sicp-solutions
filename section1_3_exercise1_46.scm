(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough? guess)
	  guess
	  (iter (improve guess))))
    (iter guess)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 2) ;=> 1.4142...
(sqrt 5) ;=> 2.236...
(sqrt 9) ;=> 3.000...

(define (fixed-point f guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) 0.00001))
  ((iterative-improve good-enough? f) guess))

(fixed-point cos 1.0) ;=> 0.739...

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0) ;=> 1.258...