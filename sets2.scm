; SETS AS ORDERED LISTS

(define nil '())

; best-case  O(1)
; avg-case   O(n/2)
; worst-case O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)	; since set is ordered, can stop searching earlier
	(else (element-of-set? x (cdr set)))))

; best-case   O(1)
; avg-case    O(n/2)
; worst-case  O(n)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< x (car set)) (cons x set))
	((= x (car set)) set)
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))



; worst-case O(n) [size of set1 + size of set2]
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond	((= x1 x2)
		 (cons x1 (intersection-set (cdr set1) (cdr set2))))
		((< x1 x2)
		 (intersection-set (cdr set1) set2))
		(else
		 (intersection-set set1 (cdr set2)))))))

; worst-case O(n) [size of set1 + size of set2]
(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      set2
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (union-set (cdr set1) (cdr set2))))
	      ((< x1 x2)
	       (cons x1 (union-set (cdr set1) set2)))
	      (else
	       (cons x2 (union-set set1 (cdr set2))))))))
	
; TEST CASES

(define digits '(0 1 2 3 4 5 6 7 8 9))
(define primes '(1 2 3 5 7 11 13 17 19))
(define odds '(1 3 5 7 9 11 13 15 17 19))
(define evens '(0 2 4 6 8 10 12 14 16 18 20))
(define power2 '(0 1 2 4 8 16 32))

(element-of-set? 4 digits)

(adjoin-set 4 '(1 3 5 7))
(adjoin-set 10 '(1 3 5 7))
(adjoin-set 0 '(1 3 5 7))	  

(intersection-set primes odds)
(intersection-set digits primes)
(intersection-set evens digits)

(union-set odds evens)
(union-set primes odds)
(union-set evens power2)


