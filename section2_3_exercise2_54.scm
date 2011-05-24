(define (equal? l1 l2)
  (cond ((eq? l1 l2) #t)
	((and (pair? l1)
	      (pair? l2)
	      (equal? (car l1) (car l2)))
	 (equal? (cdr l1) (cdr l2)))
	(else #f)))

(equal? '(this is a list) '(this is a list)) ;=> #t
(equal? '(this is a list) '(this (is a) list)) ;=> #f
(equal? '(a b c) '(c b a)) ;=> #f
(equal? '(a b) '(a b c)) ;=> #f
(equal? '() '()) ;=> #t
(equal? '(()) '(())) ;= #t
(equal? '1 '1) ;=> #t
(equal? 'a 'a) ;=> #t
(equal? '(this (is a) list) '(this (is a) list)) ;=> ?


