;Calculate a stream of approximations to pi
(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (stream-cdr s)
			    (partial-sums s))))
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1)))))
(print-n (ln2-summands 1) 10)

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(print-n ln2-stream 20)


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(print-n (euler-transform ln2-stream) 20)

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(print-n (accelerated-sequence euler-transform ln2-stream) 10)
