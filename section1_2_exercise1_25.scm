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

; Alyssa P. Hacker version...
(define (expmod base exp m)
  (remainder (expt base exp) m))

(define (even? n)
  (= (remainder n 2) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (timed-prime? n start-time)
  (cond ((fast-prime? n 10)
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

;----------------------------
; Old elapsed-time using fast-parse? (fermat test) time = O(log n)

;(search-for-primes 1000000000000)
;1000000000039 *** .01599999999996271
;1000000000061 *** 1.6000000000076398e-2
;1000000000063 *** 0.
;Value: 0

;(search-for-primes 10000000000000)
;10000000000037 *** 0.
;10000000000051 *** 0.
;10000000000099 *** 1.4999999999986358e-2
;Value: 0

;------------------------------
; Using APH version of expmod
;
;(search-for-primes 10000)
;10007 *** 2.5429999999999993
;10009 *** 2.683
;10037 *** 2.34
;Value: 0

; This is much slower for much smaller values even thought the result is right.
; That is because we're dealing with very large numbers, instead of the the other 
; expmod that takes care of complexity differently, dealing always with smaller numbers.

