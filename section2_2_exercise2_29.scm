; Binary mobiles
; --------------
; This program implements a structured representation of binary mobiles.
; A binary mobile consists of 2 branches. Each branch is a rod of a certain length.
; From the rod hangs either a weight or another binary mobile.

; Create a mobile, wich is compososed of two branches
; Branch,Branch->Mobile
(define (make-mobile left right)
  (cons left right))

; Get the left branch of a given mobile
; Mobile->Branch
(define (left-branch mobile)
  (car mobile))

; Get the right branch of a given mobile
; Mobile->Branch
(define (right-branch mobile)
  (cdr mobile))

; Mobile branches

; Create a branch, wich has a length and a structure (a weight or another mobile)
; Number,Number|Mobile->Branch
(define (make-branch length structure)
  (cons length structure))

; Get the length of the branch
; Branch->Number
(define (branch-length branch)
  (car branch))

; Get the structure of the branch (a weight or a mobile)
; Branch->Number|Mobile
(define (branch-structure branch)
  (cdr branch))

; Calculate the total weight of a binary mobile by summing the weight of it's branches
; Mobile->Number
(define (total-weight mobile)
  (if (number? mobile)  ; Check if passed value is actually a weight
      mobile            ; If true, return it's value, otherwise add it's sub-branches
      (+ (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile))))))

; Check if a mobile is balanced
; A mobile is considered balance if:
; - the total weight of the top-left branch times the length of the top-left branch;
; - is equal to the same product of the top-right branch.
; - and if the left and right branch are both balanced.
; Mobile->Boolean
(define (balanced? mobile)
  ; If value passed is actually a weight
  (if (number? mobile) 

      ; It is of course balanced
      #t
      
      ; Otherwise, check its sub-branches
      (and (= (* (branch-length (left-branch mobile))
		 (total-weight (branch-structure (left-branch mobile))))
	      (* (branch-length (right-branch mobile))
		 (total-weight (branch-structure (right-branch mobile)))))
	   (balanced? (branch-structure (left-branch mobile)))
	   (balanced? (branch-structure (right-branch mobile))))))

; TEST CASES

(define m1 (make-mobile (make-branch 1 1) (make-branch 1 1)))
(define m2 (make-mobile (make-branch 1 2) (make-branch 1 m1)))
(define m3 (make-mobile (make-branch 2 m2) (make-branch 2 m2)))
(define m4 (make-mobile (make-branch 3 m2) (make-branch 3 m3)))
(define m5 (make-mobile (make-branch 3 2) (make-branch 5 2)))
(define m6 (make-mobile
	    (make-branch 2 (make-mobile
			    (make-branch 1 3)
			    (make-branch 3 1)))
	    (make-branch 1 (make-mobile

			    (make-branch 3 2)
			    (make-branch 1 6)))))
			 
(total-weight m1) ;=> 2
(total-weight m2) ;=> 4
(total-weight m3) ;=> 8
(total-weight m4) ;=> 12
(total-weight m5) ;=> 4
(total-weight m6) ;=> 12

; TEST CASES
	
(balanced? m1) ;=> #t
(balanced? m2) ;=> #t
(balanced? m3) ;=> #t
(balanced? m4) ;=> #f 
(balanced? m5) ;=> #f
(balanced? m6) ;=> #t


