(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items))
	      (list (car items)))))

(define (reverse2 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (car things)
		    answer))))
  (iter items nil))

(reverse2 (list 1 2 3 4))
(reverse2 (list 1 4 9 16 25))