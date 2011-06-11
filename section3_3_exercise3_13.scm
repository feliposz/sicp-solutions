(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; makes a circular structure where the last element's cdr
; points to the first pair of the list
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; this causes an infinite loop because the list is a loop and has no null ending
(last-pair z)
