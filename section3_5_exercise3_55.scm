(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (stream-cdr s)
			    (partial-sums s))))

(define s (partial-sums integers))

(map (lambda (x) (stream-ref s x)) '(0 1 2 3 4 5))
;1 3 6 10 15 21
