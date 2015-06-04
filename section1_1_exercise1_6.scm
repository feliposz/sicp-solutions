; Implementation of a square root function by an iterative approach
; This particular example is buggy and it's purpose is to show why
; if has to be a special syntax and not a common procedure

(define sqrt
  (lambda (x) (sqrt-iter 1.0 x)))

(define sqrt-iter
  (lambda (guess x)
    (new-if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x))))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

; A version of if by "Eva Lu Ator"
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

; Demonstrations
(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

;(sqrt 2)
;Aborting!: maximum recursion depth exceeded

; By converting if to a conventional statement (new-if), the
; evaluation of the alternate expression is done before
; the predicate is ever checked, thus resulting in an
; infinite recursion of sqrt-iter.


