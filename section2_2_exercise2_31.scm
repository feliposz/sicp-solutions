; A generic tree-map procedure that applies proc to all nodes of a tree
(define (tree-map proc tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (proc tree))
	(else (cons (tree-map proc (car tree))
		    (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))

(define mytree (list 1
		     (list 2
			   (list 3 4)
			   5)
		     (list 6 7)))

; TEST CASES

(square-tree mytree) ;=> (1 (4 (9 16) 25) (36 49))

(tree-map 1+ mytree) ;=> (2 (3 (4 5) 6) (7 8))

(tree-map (lambda (x) (expt 2 x)) mytree) ;=> (2 (4 (8 16) 32) (64 128))


