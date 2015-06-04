; Euclid's Algorithm for finding the greatest common divisor (GCD)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

; growth = O(log n)

; in applicative-order, remainder is executed 4 times for (gcd 206 40)
;(gcd 206 40)
;(gcd 40 (remainder 206 60))
;(gcd 40 6)
;(gcd 6 (remainder 40 6))
;(gcd 6 4)
;(gcd 4 (remainder 6 4))
;(gcd 4 2)
;(gcd 2 (remainder 4 2))
;(gcd 2 0)
;2

; in normal-order evaluation
;(gcd 206 40)
;(gcd 40 (remainder 206 60))
;(if (= (remainder 206 60) 0) 40 (gcd 40 (remainder 40 (remainder 206 60))))
;(if (= 6 0) 40 (gcd...)))
;(gcd 40 (remainder 40 (remainder 206 60)))
;...

; as the program progresses, remainders get accumulated to the point where
; there are 4 chained remainders to be calculated as the a expressions
; also 1 additional remainder for every if clause
; 1 + 2 + 3 + 4 = 10 remainders in if clauses
; 4 accumulated remainders on the last step to calculate return
; 14 remainders... i guess

