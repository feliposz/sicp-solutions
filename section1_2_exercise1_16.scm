; Iterative exponetiation (logarithmic version)
; time = O(log n)
; space = O(1)

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
	(else  (fast-expt-iter b (- n 1) (* a b)))))

(fast-expt-i 2 10)

	  
      
	




