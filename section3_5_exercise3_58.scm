(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(print-n (expand 1 7 10) 10)
;1 4 2 8 5 7 1 4 2 8 

(print-n (expand 3 8 10) 10)
;3 7 5 0 0 0 0 0 0 0 
