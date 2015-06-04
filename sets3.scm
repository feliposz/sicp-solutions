; SETS AS TREES

(define nil '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (display-tree tree depth)
  (cond ((null? tree) #f)
	(else (display-tree (left-branch tree) (+ depth 1))
	      (indent depth "    ") (display (entry tree)) (newline)
	      (display-tree (right-branch tree) (+ depth 1)))))

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
			
				   

(element-of-set? 4 digits)
(element-of-set? 10 digits)
(element-of-set? 2 evens)
(element-of-set? 2 odds)
(element-of-set? 13 evens)
(element-of-set? 13 odds)

(display-tree (adjoin-set 10 primes) 0)

(define unbalanced
  (adjoin-set 4 (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 (adjoin-set 0 nil))))))

;(display-tree unbalanced 0)
;0
;    1
;        2
;            3
;                4
;Value: #f
