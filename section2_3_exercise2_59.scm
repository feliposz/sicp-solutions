(define nil '())

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
	
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

; TEST CASES

(define digits '(0 1 2 3 4 5 6 7 8 9))
(define vowels '(a e i o u))
(define consonants '(b c d f g h j k l m n p q r s t v w x y z))
(define felipo '(f e l i p o))
(define ao '(a o))
(define letters '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define primes '(1 2 3 5 7 11 13))
(define odds '(1 3 5 7 9 11 13))
(define evens '(0 2 4 6 8 10))

(union-set odds evens)
(union-set ao vowels)
(union-set vowels felipo)
(union-set consonants vowels)

