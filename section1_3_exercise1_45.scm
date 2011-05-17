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


(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) (lambda (x) x)) ; Apply zero times? Return value of x
	((= n 1) f)              ; 1 Time is the function itself
	(else                    ; Else, apply 1 time and recurse into
	 (lambda (x)             ; repeated n-1
	   (f ((repeated f (- n 1)) x))))))

; Calculate the Nth root of a given number, by applying average-damp the
; appropriate number of times for degree > 3

(define (anyroot x degree)
  (if (< degree 1)
      (error "sorry, not REALLY any root:" degree))
  (fixed-point ((repeated average-damp (if (<= degree 3)
					   1
					   (floor (/ (log (+ degree 1)) (log 2)))))
		(lambda (y) (/ x (expt y (- degree 1)))))
	       1.0))

(anyroot (expt 7.12345 32) 32) ;=> 7.12345000...





