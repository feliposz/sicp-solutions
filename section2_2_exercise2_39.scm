(define (reverse-fr sequence)
  (fold-right (lambda (x y)
		(append y (list x)))
	      nil
	      sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y)
	       (append (list y) x))
	     nil
	     sequence))

(reverse-fr (list 1 2 3 4 5))
(reverse-fl (list 1 2 3 4 5))