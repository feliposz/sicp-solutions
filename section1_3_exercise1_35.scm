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

; Calculate the golden-ratio by the fixed-point transformation of x -> 1 + 1/x

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(display golden-ratio)



