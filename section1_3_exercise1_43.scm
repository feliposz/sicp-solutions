(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) (lambda (x) x)) ; Apply zero times? Return value of x
	((= n 1) f)              ; 1 Time is the function itself
	(else                    ; Else, apply 1 time and recurse into
	 (lambda (x)             ; repeated n-1
	   (f ((repeated f (- n 1)) x))))))

((repeated square 0) 2)
;Value: 2

((repeated square 1) 2)
;Value: 4

((repeated square 2) 2)
;Value: 16

((repeated square 3) 2)
;Value: 256

((repeated square 4) 2)
;Value: 65536

((repeated square 5) 2)
;Value: 4294967296




