(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

;What will happen if the following is evaluated
; (f f)
;
;Clarifying the definition of f
; (define f (lambda (g) (g 2)))
;
;The expression (f f) turns into:
; ((lambda (g) (g 2) (lambda (g) (g 2))
;           ^   ^    \_______________/
;           |   |            |
;           \___\____________/
;The paremeter lambda is replaced in the body of the procedure
; ((lambda (g) (g 2)) 2)
;           ^         |
;           |_________|
;The parameter 2 is replaced in the body of the procedure
; (2 2)
;  ^
;  |__ 2 is not applicable


