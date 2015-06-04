; I have no idea how to solve this one...
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (even? x)
  (= (remainder x 2) 0))

(define (fib-iter a b p q count)
  (cond  ((= count 0) b)
	 ((even? count)
	  (fib-iter a
		    b
		    (do-something-to p)
		    (do-something-to q)
		    (/ count 2)))
	 (else
	  (fib-iter (+ (* b q) (* a q) (* a p))
		    (+ (* b p) (* a q))
		    p
		    q
		    (- count 1)))))
	  
(list (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5))



