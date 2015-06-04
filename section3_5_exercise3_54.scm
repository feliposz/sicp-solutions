(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams integers
			      factorials)))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams integers
			      ones)))

(map (lambda (x) (stream-ref factorials x)) '(0 1 2 3 4 5 6 7 8 9 10))
;Value 40: (1 1 2 6 24 120 720 5040 40320 362880 3628800)