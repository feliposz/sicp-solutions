(define x (list (list 1 2) (list 3 4)))

(reverse x) ;=> ((3 4) (1 2))

(define (deep-reverse items)
  (define (iter things answer)
    (cond ((null? things) answer)
	  ((not (pair? things)) things)
	  (else (iter (cdr things)
		      (cons (deep-reverse (car things))
			    answer)))))
  (iter items nil))

(deep-reverse x) ;=> ((4 3) (2 1))

      
      