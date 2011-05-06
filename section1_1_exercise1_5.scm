; This test will determine if the interpreter is using a
; applicative-order evaluation or a normal-order

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
; C-c C-C will stop the interpreter

; If applicative-order is used, the value of (p) will be
; evaluated before the substitution of test.
; Since (p) is defined recursively, the evaluation never ends.
;
;(test 0 (p))
;(test 0 (p))
;(test 0 (p))
;(test 0 (p))
; .....
;
; If normal-order is used, test is substituted first and since
; x is 0, the <consequent> expression of if is evaluated and
; the (p) expression (the <alternate>) is never evaluated.
;
;(test 0 (p))
;(if (= x 0) 0 y))
;(if (= 0 0) 0 (p))
;(if #t 0 (p))
;0
