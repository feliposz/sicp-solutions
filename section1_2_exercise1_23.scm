(define (smallest-divisor n)
  (find-divisor n 2))

(define (next x)
  (if (= x 2)
      3
      (+ x 1)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (timed-prime? n start-time)
  (cond ((prime? n)
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
; Old elapsed-time values for the procedure when testing for even numbers

;(search-for-primes 1000000000000)
;1000000000039 *** 7.612999999999943
;1000000000061 *** 8.627999999999929
;1000000000063 *** 11.169000000000096
;Value: 0

;(search-for-primes 10000000000000)
;10000000000037 *** 21.29499999999996
;10000000000051 *** 19.187000000000012
;10000000000099 *** 23.883000000000038
;Value: 0

;----------------------------
; New elapsed-time for function, now skipping even numbers

;(search-for-primes 1000000000000)
;1000000000039 *** 6.912000000000035
;1000000000061 *** 5.100999999999999
;1000000000063 *** 8.392999999999915
;Value: 0

;(search-for-primes 10000000000000)
;10000000000037 *** 37.81600000000003
;10000000000051 *** 23.760999999999967
;10000000000099 *** 20.701999999999998
;Value: 0

; Not much has changed. Apparently it's the same time.
; I guess that the added test inside the (next) procedure is taking the same
; time for the test that the even numbers would take. (???)
