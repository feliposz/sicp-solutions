; Call the procedure proc for each element of items
; Returns #t
(define (my-for-each proc items)
  (cond ((null? items) #t)
	(else (proc (car items))
	      (my-for-each proc (cdr items)))))

(my-for-each (lambda (x) (display x) (newline))
	     (list 57 321 88))
	   