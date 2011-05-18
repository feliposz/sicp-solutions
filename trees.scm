(define nil ())

(define (leaf? x)
  (not (pair? x)))

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((leaf? tree) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))

(count-leaves (list 2)) ;=> 1

(count-leaves (list 5 7)) ;=> 2

(define my-tree (list 4 (list 5 7) 2))

(count-leaves my-tree) ;=> 4

; A simple implementation of a procedur to scale the values in the tree
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

; Another implementation that considers tree to be a list of subtrees and uses map
; The base factor is the leaf
(define (scale-tree tree factor)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (scale-tree subtree factor)
	     (* subtree factor)))
       tree))

; A generic procedure to apply operations to members of a tree
(define (tree-map proc tree)
  (cond ((null? tree) nil)
	((leaf? tree) (proc tree))
	(else (cons (tree-map proc (car tree))
		    (tree-map proc (cdr tree))))))

(tree-map 1+ my-tree) ;=> (5 (6 8) 3)

(define other-tree (list 1 (list 2 (list 3 4) 5) 6))

; A generic procedure to manipulate a tree
(define (tree-manip leaf-op init merge tree)
  (cond ((null? tree) init)
	((leaf? tree) (leaf-op tree))
	(else (merge (tree-manip leaf-op init merge (car tree))
		     (tree-manip leaf-op init merge (cdr tree))))))

; Similar to map, reconstruct tree squaring elements
(tree-manip
 square ; will apply square to each element
 nil    ; return nil when found a nil element (when reconstructing tree)
 cons   ; reconstruct tree in the same maner
 other-tree) => (1 (4 (9 16) 25) 36)

; Count leaves of the tree
(tree-manip
 (lambda (x) 1) ; each element will count as 1 in the sum
 0              ; nil nodes are not counted
 +              ; instead of recombining trees, sum the results of applying leaf-op (1)
 other-tree) => 6

; Reverse the tree
(tree-manip
 (lambda (x) x) ; identity procedure - return itself
 nil            ; reconstruct nil as usual
 (lambda (a b)  ; recombine the nodes in reverse order
   (append b (list a)))
 other-tree)
   
; Accumulation procedure

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
	    (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define tree1 (list 1 (list 2 (list 3 4)) 5)))

(enumerate-tree tree1)

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(sum-odd-squares tree1)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(even-fibs 10)

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(list-fib-squares 10)