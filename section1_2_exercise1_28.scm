; Fermat Test for prime-numbers
; This is probabilistic test!!!

; !!! Improved with the Miller-Rabin test !!!

; TODO: need to check if expmod returned 0
(define (fermat-test n)
  (define (try-it a)
    (and (= (expmod a n n) a)
	 (not (= (expmod a (- n 1) n) 0))))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;TODO: check for non-trivial square-root of 1 modulo n,
; that is, a number not equal to 1 or n - 1 whose square is equal to 1 modulo n
; I HAVE NO IDEA HOW TO DO THIS!!!!
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





