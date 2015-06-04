(define ones (cons-stream 1 ones))
(define zeroes (cons-stream 0 zeroes))

(define integers (cons-stream 1
			      (add-streams integers ones)))

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
	((empty-stream? s2) s1)
	(else
	 (cons-stream
	  (+ (head s1) (head s2))
	  (add-streams (tail s1) (tail s2))))))

(define (sub-streams s1 s2)
  (stream-map - s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (mul-streams (div-streams ones integers) s))
	
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (sub-streams zeroes sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(print-n exp-series 10)
(print-n cosine-series 10)
(print-n sine-series 10)




; Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) ;???
	       (add-streams (stream-cdr s1) (stream-cdr s2))))

; I guess...
(print-n (mul-series cosine-series sine-series) 10)

; Exercise 3.61 ???????

(define (invert-unit-series s)
  (cons-stream (stream-car s)
	       (sub-streams zeroes (mul-series (stream-cdr s)
					       (invert-unit-series s)))))

(print-n (invert-unit-series sine-series) 10)

; Exercise 3.62

(define (div-series s1 s2)
  (let ((inverted (invert-unit-series s2)))
    (if (= (stream-car inverted) 0)
	(error "Constant term of divisor is zero")
	(mul-series s1 inverted))))

(print-n (div-series cosine-series cosine-series) 10)
