(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) (lambda (x) x)) ; Apply zero times? Return value of x
	((= n 1) f)              ; 1 Time is the function itself
	(else                    ; Else, apply 1 time and recurse into
	 (lambda (x)             ; repeated n-1
	   (f ((repeated f (- n 1)) x))))))

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

; Apply smooth 3 times to the function square
; and apply the 3-times-smoothed-square function to 2
(((repeated smooth 3) square) 2)
