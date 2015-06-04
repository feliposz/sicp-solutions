; Return the list with it's elements squared
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
	    (square-list (cdr items)))))

; Same thing using map
(define (square-list-map items)
  (map square items))

(square-list (list 1 2 3 4 5))
(square-list-map (list 1 2 3 4 5))
