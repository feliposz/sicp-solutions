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

; Louis reasoner version
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/ exp 2) m))
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

;----------------------------
; Elapsed-time using fast-parse? (fermat test) but with Louis Reasoner version of expmod

;(search-for-primes 10000)

10007 *** .6699999999999982
10009 *** .5309999999999988
10037 *** .39000000000000057
;Value: 0

; Much slower than other version. That's because (expmod) is now O=(n) instead of O=(log n)
; since (expmod) is recursed 2 times with the same paramers instead and then squared.

