;implementations of the function f(n)
;f(n)
;| n                           if n < 3
;| f(n-1) + 2f(n-2) + 3f(n-3)  if n>= 3

;recursive
(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1))
	 (* 2 (f-r (- n 2)))
	 (* 3 (f-r (- n 3))))))


;iterative
(define (f-i n)
  (f-iter 0 1 2 0 n))

(define (f-iter f f+1 f+2 c n)
  (if (= c n)
      f
      (f-iter f+1 f+2
	      (+ f+2 (* 2 f+1) (* 3 f))
	      (+ c 1) n)))

(f-r 3)
(f-i 3)
;4
(f-r 4)
(f-i 4)
;11
(f-r 5) 
(f-i 5)
;25
(f-r 6)
(f-i 6)
;59
(f-r 7)
(f-i 7)
;142

; slow!!!
(f-r 25)
(f-i 25)
;812934961

