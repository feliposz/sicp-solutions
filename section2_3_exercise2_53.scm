(list 'a 'b 'c)
;Value 12: (a b c)

(list (list 'george))
;Value 13: ((george))

(cdr '((x1 x2) (y1 y2)))
;Value 14: ((y1 y2))

(pair? (car '(a short list)))
;Value: #f

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))
;Value: memq

(memq 'red '((red shoes) (blue socks)))
;Value: #f

(memq 'red '(red shoes blue socks))
;Value 15: (red shoes blue socks)
