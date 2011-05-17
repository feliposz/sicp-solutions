; Method for finding fixed-point of (some) functions, by successively
; applying the function until the value doesn't change much.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;-- Newton's Method ---------------

(define dx 0.00001)

; Return an approximate of the derivative of funtion g()
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;-- Exercise 1.40 -----------------

(define (cubic a b c) 
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(newtons-method (cubic 2 2 -14) 1) ; => 1.694...

(newtons-method (cubic 10 15 20) 1) ; => -8.514...

(newtons-method (cubic -0.1 0.2 -0.3) 1) ; => 0.6

