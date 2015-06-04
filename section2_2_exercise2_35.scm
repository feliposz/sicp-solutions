(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (t)
		     (if (not (pair? t))
			 1
			 (count-leaves t)))
		   t)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(count-leaves (list 1 (list 2 (list 3 4)) 5))
(count-leaves (list 1 2 3))
(count-leaves (list 4 (list 5 7) 2))
(count-leaves (list 1 (list 2 (list 3 4 ) (list 5 6) (list 7 8 9)) (list 10 11)
		    (list 12 13 14) (list (list 15 16 (list 17 (list (list 18)))))))