; Fermat Test for prime-numbers
; This is probabilistic test!!!

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (even? n)
  (= (remainder n 2) 0))



(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (timed-prime? n start-time)
  (cond ((fast-prime? n 3)
	 (newline)
	 (display n)
	 (report-prime (- (runtime) start-time)) #t)
	(else #f)))

(define (search-for-primes n)
  (define (search-loop n count)
    (if (= count 0)
	0
	(search-loop (+ n 1) (if (timed-prime? n (runtime))
				 (- count 1)
				 count))))
  (search-loop n 3))

;---------------------------------
; Old elapsed-time values for parse? time = O(sqrt n)

(search-for-primes 1000000000000)
;1000000000039 *** 7.612999999999943
;1000000000061 *** 8.627999999999929
;1000000000063 *** 11.169000000000096
;Value: 0

(search-for-primes 10000000000000)
;10000000000037 *** 21.29499999999996
;10000000000051 *** 19.187000000000012
;10000000000099 *** 23.883000000000038
;Value: 0

;----------------------------
; New elapsed-time using fast-parse? (fermat test) time = O(log n)


