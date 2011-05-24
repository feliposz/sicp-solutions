; THIS VERSION ALLOWS FOR DUPLICATES

; In this version, adjoin-set and union-set are faster operations, because there
; is no need to check if an element is already on the set.
;
; Order of growth:
; adjoin-set is O(1) => no-duplicates version is O(n)
; union-set is O(n)  => no-duplicates version is O(n^2)
; element-of-set? is still O(n)
; intersection-set is still O(n^2)
;
; Nevertheless, checking for elements may take a longer time for
; very large sets, because duplicates would make searching slower.
;
; This implementation is better than the no-duplicates version if
; adjoin-set and union-set are much more frequent than
; intersection-set and element-of-set?
;
; If intersection-set and element-of-set are more frequently used,
; than adjoin-set and union-set will keep the resulting sets smaller
; for searching.

(define nil '())

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
	
(define (union-set set1 set2)
  (append set1 set2))


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

(intersection-set felipo vowels)
(intersection-set felipo consonants)
(intersection-set consonants vowels)
(intersection-set primes odds)

(union-set odds evens)
(union-set ao vowels)
(union-set vowels felipo)
(union-set consonants vowels)

