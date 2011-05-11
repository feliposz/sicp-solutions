(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

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

; sum of time taken for 10000000000000 divided by sum of 1000000000000
(/ (+ 21.295 19.187 23.883) (+ 7.613 8.628 11.169))
;Value: 2.348230572783655

(sqrt 10)
;Value: 3.1622776601683795

; The time taken to calculate seems to be growing closely to (sqrt 10).
; It appears to triple for each trailing 0 added.
; Fluctuations on processor load could be interfering with exact time taken.

;(search-for-primes 1000000)
;1000003 *** 1.5000000000100044e-2
;1000033 *** 0.
;1000037 *** .01599999999996271
;Value: 0

;(search-for-primes 10000000)
;10000019 *** .03199999999992542
;10000079 *** .03099999999994907
;10000103 *** 3.1000000000062755e-2
;Value: 0

 
;(search-for-primes 100000000)
;100000007 *** .09399999999993724
;100000037 *** .11000000000001364
;100000039 *** .1089999999999236
;Value: 0

;(search-for-primes 1000000000)
;1000000007 *** .34399999999993724
;1000000009 *** .37400000000002365
;1000000021 *** .35800000000006094
;Value: 0

;(search-for-primes 10000000000)
;10000000019 *** .6549999999999727
;10000000033 *** .6709999999999354
;10000000061 *** .6240000000000236
;Value: 0

;(search-for-primes 100000000000)
;100000000003 *** 3.744000000000028
;100000000019 *** 3.7749999999999773
;100000000057 *** 3.744000000000028
;Value: 0

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

