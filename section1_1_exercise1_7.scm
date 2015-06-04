; Improved version of sqrt. Works for larg numbers.
; Still have problems for small numbers thought.

(define sqrt
  (lambda (x) (sqrt-iter 1.0 x)))

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess (improve guess x))
	guess
	(sqrt-iter (improve guess x) x))))

; Test how much guess has improved
(define good-enough?
  (lambda (guess improved)
    (< (abs (- guess improved)) 0.0000001)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

; Better...
(sqrt 10000000000000000000000)

; wrong answer because value is too small
; need to reduce constant too much
(square (sqrt 1.0e-14))

; This improved answer for very large numbers but doesn't
; seem to affect small numbers




