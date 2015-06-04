; linear recusive multiplication
; This is known as the ancient Egyptian method of multiplication!!!
; space / time = O(n)

(define (mult a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(mult 3 7)

; ------------------

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mult-i a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b x)
  (cond ((= b 0) x)
	((even? b) (fast-mult-iter (double a) (halve b) x))
	(else (fast-mult-iter a (- b 1) (+ x a)))))

(fast-mult-i 10 15)
(fast-mult-i 2 3)
(fast-mult-i 7 7)
(fast-mult-i 0 3)
(fast-mult-i 3 0)
(fast-mult-i 20 30)

; a iterative multiplication method with logarithmic time and constant space
; time = O(log n)
; space = O(1)



