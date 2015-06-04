; linear recusive multiplication
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

(define (fast-mult a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-mult a (halve b))))
	(else (+ a (fast-mult a (- b 1))))))

(fast-mult 10 15)

; a recursive multiplication method with logarithmic time and space
; space / time = O(log n)


