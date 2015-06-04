;Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(a 1 10)
;1024

(a 2 4)
; 65536

(a 3 3)
; 65536

;A(x, y)
;| y = 0 -> 0
;| x = 0 -> 2 * y
;| y = 1 -> 2
;| else  -> A(x-1, A(x, y-1))

(define (f n) (a 0 n))

;f(n) -> 2 * n

(define (g n) (a 1 n))

;g(n)
;|n = 0 -> 0
;|n > 1 -> 2 ^ n

(define (h n) (a 2 n))

(h 3)

;h(n)
;|n = 0 -> 0
;|n = 1 -> 2
;|n > 1 -> 2 ^ n ^ n ???

(define (k n) (* 5 n n))

;k(n) -> 5n^2
