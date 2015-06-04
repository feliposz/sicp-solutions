(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; It is possible for an operand of a combination to be an
; expression and it's evaluated as any other expression.
; It's return value must be a valid operation/symbol.
; When the predicate of if is evaluated, it will return the
; operand + (consequent) or the operand - (alternate) and the
; expression (+ a b) or (- a b) will be evaluated.
; This is possible because + and - are just names/symbols for
; operations on lisp/scheme.
