(define (count-pairs x)
  (let ((pairs-found '()))
    (define (insert-pair p) ; insert new pair at the beginning of the list
      (let ((new (cons p pairs-found)))
        (set! pairs-found new)))
    (define (has-pair? p) ; check if list already has the given pair
      (define (search list) ; loop through the list
        (cond ((null? list) #f) ; end of the list, not found
              ((eq? p (car list)) #t) ; the one, we are looking for
              (else (search (cdr list))))) ; check the rest
      (search pairs-found))
    (define (check-pairs x)
      (if (pair? x) ; only count pairs and sub-pairs
          (begin
            (if (not (has-pair? x)) ; check if already counted
                (insert-pair x) ; first time pair is found, add it to list
                0) ; do nothing
            (check-pairs (car x))  ; count sub-structures
            (check-pairs (cdr x))) ; count sub-structures
          0)) ; do nothing
    (check-pairs x)
    (length pairs-found)))

(define a (cons 1 2))
(define b (cons a a))
(define c (cons 1 b))
(define d (cons b b))
(define e (cons 1 2))
(set-cdr! e e)

a
b
c
d
;e

(count-pairs a) ; 1 pair
(count-pairs b) ; 2 pairs
(count-pairs c) ; 3 pairs
(count-pairs d) ; 3 pairs
;(count-pairs e)
;e is a circular self-referencing structure
;count-pairs never returns
