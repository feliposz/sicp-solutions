; Get the square of a number
(define (square x)
  (* x x))

; Sum the square of 2 numbers
(define (sum-square x y)
  (+ (square x) (square y)))

; Sum the square of the 2 largest number out of 3
(define (sum-square-of-largers x y z)
  (cond ((= (max3 x y z) x) (sum-square x (max2 y z)))
	((= (max3 x y z) y) (sum-square y (max2 x z)))
	((= (max3 x y z) z) (sum-square z (max2 x y)))))


; Get the largest of 2 values
(define (max2 a b)
  (if (> a b)
      a
      b))

; Get the largest of 3 values
(define (max3 a b c)
  (max2 a (max2 b c)))

;----------------------------------
; Tests

(sum-square-of-largers 4 2 3)
;expected 4^2 + 3^2 = 25

(sum-square-of-largers 0 1 2)
;expected 2^2 + 1^2 = 5

(sum-square-of-largers 20 3 4)
;expected 20^2 + 4^2 = 416

