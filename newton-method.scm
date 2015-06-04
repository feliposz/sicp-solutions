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

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(cbrt (* 10 10 10))



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

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(define (cube x) (* x x x))

(define (cube-root x)
  (newtons-method (lambda (y) (- (cube y) x))
		  1.0))

(sqrt 2)

(cube-root 27)

; -- Generalized fixed point of transform ---------------

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; redefinitions:

; using average-damp as transform of y->x/y
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

; using newton-transform as transform of y->y^2-x
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x) newton-transform 1.0)))

