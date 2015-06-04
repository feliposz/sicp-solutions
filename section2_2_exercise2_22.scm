; Return the list with it's elements squared
; Iterative implementation
; BUG: Returns a reversed list

(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))

(square-list-1 (list 1 2 3 4 5)) ;=> (25 16 9 4 1)

; 1) The problem is that the cons operation as used in the first version
; is always inserting the square of the current element at the beginning
; of the list, and not appending to it.

(square-list-2 (list 1 2 3 4 5)) ;=> (((((() . 1) . 4) . 9) . 16) . 25)

; 2) Simply reversing the order of arguments to cons doesn't solve the
; problem, because to construct lists one has to consider the car part
; of a pair as the value and the cdr part as a pointer to the next pair.
; When the order is reversed, instead of a list, the result is a sequence
; of pairs.

