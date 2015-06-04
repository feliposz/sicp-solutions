; recursive solution to hanoi tower
(define (move n from to spare)
  (cond ((= n 0) "done")
        (else 
         (move (- n 1) from spare to)
         (print-move from to)
         (move (- n 1) spare to from))))

; print movements
(define (print-move from to)
  (display "move from ")
  (display from)
  (display " to ")
  (display to)
  (display "\n"))

(move 3 "A" "B" "C")







