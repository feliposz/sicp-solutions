; best-case   O(1)
; avg-case    O(n/2)
; worst-case  O(n)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< x (car set)) (cons x set))
	((= x (car set)) set)
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))
