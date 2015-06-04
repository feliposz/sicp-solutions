; Define the terminator for a list (an empty list)
(define nil ())

; Construct a list with pairs ponting to the next
(cons 1
      (cons 2
	    (cons 3
		  (cons 4 nil))))

; Construct a list with the list primitive
(list 1 2 3 4)

; Attribute list to a variable
(define one-through-four (list 1 2 3 4))

; Get the first, second, third and fourth element of a list
(car one-through-four)
(car (cdr one-through-four))
(car (cdr (cdr one-through-four)))
(car (cdr (cdr (cdr one-through-four))))

; Shortcuts for getting the second, third and fourth elements (two versions)
(cadr one-through-four)
(caddr one-through-four)
(cadddr one-through-four)
(car (cdddr one-through-four))

; Return a list with the element 10 at the beginning
(cons 10 one-through-four)

; Get the Nth item of a list (0 is the first element, 3 is the 4th, etc.)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

; Return the fourth element
(list-ref one-through-four 3)

; Count the number of elements in a list (recursively)
; NOTE: There is a built-in function for this named length
(define (length-r items)
  (if (null? items)
      0
      (+ 1 (length-r (cdr items)))))

(length-r one-through-four)

; Iterative version of length
(define (length-i items)
  (define (iter a count)
    (if (null? a)
	count
	(iter (cdr a) (+ count 1))))
  (iter items 0))

(length-i one-through-four)

; Combines two lists in a new
; NOTE: the same as buil-in procedure append
(define (myappend list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1) list2))))

(define tens (list 10 20 30 40 50))

(myappend one-through-four tens)

; Apply procedure (proc) for all elements of list and return a new list with results
; NOTE: the same as built-in procedure map
(define (mymap proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (mymap proc (cdr items)))))

(mymap abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)




