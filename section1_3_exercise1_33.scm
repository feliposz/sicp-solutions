; A generalized version of an accumulator with a filter option

(define (filtered-accumulate combiner filter? null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter? a)
		    (term a)
		    null-value)
		(filtered-accumulate
		 combiner filter? null-value term (next a) next b))))

; TEST CASES -----------------------------------------

;Sum the even numbers between 1 and 10 => 2 + 4 + 6 + 8 + 10 = 30
(filtered-accumulate + even? 0 identity 1 1+ 10)

;Product of the odd numbers between 1 and 10 => 1 * 3 * 5 * 7 * 9 = 945
(filtered-accumulate * odd? 1 identity 1 1+ 10)

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(define (sum-primes a b)
  (filtered-accumulate + prime? 0 square a 1+ b))

; 1 + 2^2 + 3^2 + 5^2 + ... + 17^2 + 19^2 = 1028
;(sum-primes 1 20) => 1028  (OK!)

(define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))
 
(define (product-relative-primes n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * relative-prime? 1 identity 1 1+ n))

; 1 * 3 * 7 * 9 = 189
;(product-relative-primes 10) => 189 (OK)

; 1 * 2 * 4 * 7 * 8 * 11 * 13 * 14 = 896896
;(product-relative-primes 15) => 896896 (OK)



