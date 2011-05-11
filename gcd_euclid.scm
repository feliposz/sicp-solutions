; Euclid's Algorithm for finding the greatest common divisor (GCD)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(gcd 16 28)

; growth = O(log n)
