(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; makes a circular structure where the last element's cdr
; points to the first pair of the list
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define y (list 'a 'b 'c))
(define z (make-cycle (list 'a 'b 'c) ))

(define (check-cycle lst)
  (define (contains? lst element)
    (cond ((null? lst) #f)
          ((eq? element (car lst)) #t)
          (else (contains? (cdr lst) element))))
  (define (loop lst previous)
    (cond ((null? lst) #f)
          ((contains? previous (cdr lst)) #t)
          (else (loop (cdr lst) (cons (cdr lst) previous)))))
  (loop lst '()))

(check-cycle y)
(check-cycle z)

;TODO: try to implement using constant space O(1) for exercise 3.19
