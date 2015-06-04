(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

v
w

; Mystery is a procedure that reverses the given list in place and
; returns a pointer to the first element.

; v is left pointing to the last element
; w is left pointing to the beginning of the list