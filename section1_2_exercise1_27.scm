; CHECK BELOW.........

; Fermat Test for prime-numbers
; This is probabilistic test!!!

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (even? n)
  (= (remainder n 2) 0))




;(prime? 561)
;Value: #f

;(prime? 1105)
;Value: #f

;(prime? 1729)
;Value: #f

;(prime? 2465)
;Value: #f

;(prime? 2821)
;Value: #f

;(prime? 6601)
;Value: #f


(fast-prime? 561 10)
;Value: #t

(fast-prime? 1105 10)
;Value: #t

(fast-prime? 1729 10)
;Value: #t

(fast-prime? 2465 10)
;Value: #t

(fast-prime? 2821 10)
;Value: #t

(fast-prime? 6601 10)
;Value: #t

; In fact, as shown, Fermat test fails on the Carmichael numbers.