; Procedures

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
	((empty-stream? s2) s1)
	(else
	 (cons-stream
	  (+ (head s1) (head s2))
	  (add-streams (tail s1) (tail s2))))))

(define (scale-stream c s)
  (stream-map (lambda (x) (* x c))
	      s))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream dt integrand)
			      int)))
  int)

; Exercise 3.73

; RC circuit
(define (rc res cap dt)
  (lambda (s)
    (add-streams (scale-stream res s)
		 (integral (scale-stream (/ 1 cap) s)
			   (stream-car s)
			   dt))))

(define rc1 (rc 5 1 0.5))

;(print-n (rc1 integers) 10)




; Exercise 3.74

; Original
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

; Map version
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))



; Exercise 3.75

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
		 (make-zero-crossings (stream-cdr input-stream)
				      (stream-car input-stream)
				      avpt))))


; Exercise 3.76

(define (average x y)
  (/ (+ x y) 2))

(define (smooth-stream proc s)
  (define (smoother s last)
    (cons-stream (proc (stream-car s) last)
		 (smoother (stream-cdr s) (stream-car s))))
  (smoother s 0))

(define smoothed-sense-data (smooth-stream average sense-data))

(define zero-crossings
  (stream-map sign-change-detector smoothed-sense-data
	      (cons-stream 0 smoothed-sense-data)))






