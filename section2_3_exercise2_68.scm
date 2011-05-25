(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree (make-leaf 'B 2)
				  (make-code-tree (make-leaf 'D 1)
						  (make-leaf 'C 1)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-1 subtree)
    (if (leaf? subtree)
	'()
	(let ((left (left-branch subtree))
	      (right (right-branch subtree)))
	  (cond ((element-of-list? symbol (symbols left))
		 (cons 0 (encode-1 left)))
		((element-of-list? symbol (symbols right))
		 (cons 1 (encode-1 right)))
		(else (error "invalid symbol to encode ENCODE-SYMBOL:" symbol))))))
  (encode-1 tree))

(define (element-of-list? x list)
  (cond ((null? list) #f)
	((eq? x (car list)) #t)
	(else (element-of-list? x (cdr list)))))

(encode '(a d a b b c a) sample-tree) ;=> (0 1 1 0 0 1 0 1 0 1 1 1 0)







