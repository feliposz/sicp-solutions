(define (last-pair items)
  (cond ((null? items) nil)
	((null? (cdr items)) items)
	(else (last-pair (cdr items)))))

(last-pair (list 23 72 149 34))
