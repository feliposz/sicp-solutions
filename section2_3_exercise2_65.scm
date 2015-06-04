; SETS AS TREES

(define nil '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (display-tree tree depth)
  (cond ((null? tree) #f)
	(else (display-tree (right-branch tree) (+ depth 1))
	      (indent depth "    ") (display (entry tree)) (newline)
	      (display-tree (left-branch tree) (+ depth 1)))))

(define (indent depth string)
  (cond ((= depth 0) #f)
	(else (display string)
	      (indent (- depth 1) string))))

; Searching is O(log n) if tree is balanced
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set)) (element-of-set? x (left-branch set)))
	((> x (entry set)) (element-of-set? x (right-branch set)))))

; Adding is O(log n) if tree is already balanced,
; but adding elements may unbalance a tree
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))



; TEST CASES

(define odds
  (make-tree 7
	     (make-tree 3
			(make-tree 1 nil nil)
			(make-tree 5 nil nil))
	     (make-tree 11
			(make-tree 9 nil nil)
			(make-tree 13 nil nil))))

(define evens
  (make-tree 6
	     (make-tree 2
			(make-tree 0 nil nil)
			(make-tree 4 nil nil))
	     (make-tree 10
			(make-tree 8 nil nil)
			(make-tree 12 nil nil))))

(define digits
  (make-tree 7
	     (make-tree 3
			(make-tree 1
				   (make-tree 0 nil nil)
				   (make-tree 2 nil nil))
			(make-tree 5
				   (make-tree 4 nil nil)
				   (make-tree 6 nil nil)))
	     (make-tree 9
			(make-tree 8 nil nil)
			nil)))
			
(define primes
  (make-tree 11
	     (make-tree 5
			(make-tree 2
				   (make-tree 1 nil nil)
				   (make-tree 3 nil nil))
			(make-tree 7 nil nil))
	     (make-tree 17
			(make-tree 13 nil nil)
			(make-tree 19 nil nil))))
			
(define (tree->list-1 tree)
  (if (null? tree)
      nil
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree nil))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))


(define (partial-tree elements n)
  (if (= n 0)
      (cons nil elements)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elements left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elements (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elements))
		  (right-result (partial-tree (cdr non-left-elements)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elements (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elements))))))))


; worst-case O(n) [size of set1 + size of set2]
(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond	((= x1 x2)
		 (cons x1 (intersection-list (cdr set1) (cdr set2))))
		((< x1 x2)
		 (intersection-list (cdr set1) set2))
		((> x1 x2)
		 (intersection-list set1 (cdr set2)))))))

; worst-case O(n) [size of set1 + size of set2]
(define (union-list set1 set2)
  (if (or (null? set1) (null? set2))
      set2
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (union-list (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (cons x1 (union-list (cdr set1) set2)))
	      ((> x1 x2)
	       (cons x2 (union-list set1 (cdr set2))))))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-1 set1))
	(list2 (tree->list-1 set2)))
    (list->tree (intersection-list list1 list2))))

(define (union-set set1 set2)
  (let ((list1 (tree->list-1 set1))
	(list2 (tree->list-1 set2)))
    (list->tree (union-list list1 list2))))


(define digits (list->tree '(0 1 2 3 4 5 6 7 8 9)))
(define primes (list->tree '(1 2 3 5 7 11 13 17 19)))
(define odds (list->tree '(1 3 5 7 9 11 13 15 17 19)))
(define evens (list->tree '(0 2 4 6 8 10 12 14 16 18 20)))
(define power2 (list->tree '(0 1 2 4 8 16 32)))

(define all (union-set odds evens))
(define digprimes (intersection-set digits primes))

(display-tree all 0)

(display-tree digprimes 0)

