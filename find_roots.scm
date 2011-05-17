; Find the root of function, given 2 points where:
; f(neg-point) < 0 < f(pos-point)

(define tolerance 0.001)

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value) (search f neg-point midpoint))
		((negative? test-value) (search f midpoint pos-point))
		(else midpoint))))))

; time/space = O(log (interval/tolerance)) where interval is (pos-point - neg-point)

; Make sure the given points have opposite signs
(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (positive? a-value) (negative? b-value))
	   (search f b a))
	  (else (error "Values are not of opposite signal" a b)))))


(half-interval-method sin 2.0 4.0)

(half-interval-method
 (lambda (x) 
   (- (* x x x)
      (* 2 x)
      3))
 1.0
 2.0)
	   
