(define (stream-limit s tolerance)
  (let ((current (stream-car s))
	(next (stream-car (stream-cdr s))))
    (if (< (abs (- current next)) tolerance)
	next
	(stream-limit (stream-cdr s) tolerance))))

;Calculate square root as a stream that approximates to the value
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(my-sqrt 2 0.00001)
(sqrt 2)
