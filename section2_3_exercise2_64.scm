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
  (display "Tree:")
  (car (partial-tree elements (length elements) 0)))


(define (partial-tree elements n depth)
  (indent depth "  ")
  (display "PT-> ELTS:") (display elements)
  (display " N:") (display n)
  (newline)
  (if (= n 0)
      (cons nil elements)
      (let ((left-size (quotient (- n 1) 2)))
	(indent depth "  ") (display "Left:") (newline)
	(let ((left-result (partial-tree elements left-size (+ depth 1))))
	  (let ((left-tree (car left-result))
		(non-left-elements (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (indent depth "  ") (display "Right:") (newline)
	    (let ((this-entry (car non-left-elements))
		  (right-result (partial-tree (cdr non-left-elements)
					      right-size
					      (+ depth 1))))
	      (let ((right-tree (car right-result))
		    (remaining-elements (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elements))))))))

;Call tree for (list->tree '(1 3 5 7 9 11))
;
;Tree:
;PT-> ELTS:(1 3 5 7 9 11) N:6
;Left:
;  PT-> ELTS:(1 3 5 7 9 11) N:2
;  Left:
;    PT-> ELTS:(1 3 5 7 9 11) N:0
;  Right:
;    PT-> ELTS:(3 5 7 9 11) N:1
;    Left:
;      PT-> ELTS:(3 5 7 9 11) N:0
;    Right:
;      PT-> ELTS:(5 7 9 11) N:0
;Right:
;  PT-> ELTS:(7 9 11) N:3
;  Left:
;    PT-> ELTS:(7 9 11) N:1
;    Left:
;      PT-> ELTS:(7 9 11) N:0
;    Right:
;      PT-> ELTS:(9 11) N:0
;  Right:
;    PT-> ELTS:(11) N:1
;    Left:
;      PT-> ELTS:(11) N:0
;    Right:
;      PT-> ELTS:() N:0

; This procedure divides the tree in 3 roughly equal portions:
; The left-result, the this-entry and the right-result
; The left portion is processed first to produce the entire left-branch
; of the tree recursively.
; When the left part is done, the right side is processed.
; When both parts are done, a tree is created with the left and right
; parts and the "middle" element (this-entry).
; This tree is returned along with the rest of the elements to be processed.

; The order of growth is roughly number of elements in the list times 2 plus 1.
; O(n * 2 + 1) => O(n)

; Another way to think about it is this, since each step reduce the size of
; the problem by 1, by creating a subtree with the processed element, the
; problem size grows linearly.

