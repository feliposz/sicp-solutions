; or-gate2-delay = inverter-delay * 4 + and-gate-delay * 3
(define (or-gate2 a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire))
        (f (make-wire))
        (g (make-wire))
        (h (make-wire))
        (i (make-wire))
        (j (make-wire)))
    (inverter a c)
    (and-gate a c d) ; a and not(a) = false
    (inverter d e) ; this will always be true
    (and-gate a e f)
    (and-gate b e g)
    (inverter f h)
    (inverter g i)
    (and-gate h i j)
    (inverter j output)))

; equivalent logical expression
(define (my-or a b)
  (let ((true (not (and a (not a)))))
    (not (and (not (and a true)) (not (and b true))))))

(my-or #f #f)
(my-or #f #t)
(my-or #t #f)
(my-or #t #t)