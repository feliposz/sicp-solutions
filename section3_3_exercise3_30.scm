; DUMMY versions, just for testing   
(define (full-adder a b c-in sum c-out)
  (display "FULL-ADDER A=")
  (display a)
  (display " B=")
  (display b)
  (display " C-IN=")
  (display c-in)
  (display " SUM=")
  (display sum)
  (display " C-OUT=")
  (display c-out)
  (newline))
(define (make-wire) 'wire)


(define (ripple-carry-adder alist blist slist c)
  (let ((clist (list c ))) ; the last carry will be in the end of the list
    (define (fill-clist n)
      (cond ((= n 0) 'ok)
            (else
             (set! clist (cons (make-wire) clist))
             (fill-clist (- n 1)))))                  
    (define (loop alist blist slist clist)
      (cond ((null? alist) 'ok)
            (else
             (full-adder (car alist)
                         (car blist)
                         (car clist) ;c-in = current
                         (car slist)
                         (cadr clist)) ; c-out = next
             (loop (cdr alist) (cdr blist) (cdr slist) (cdr clist)))))
    (fill-clist (length alist))
    (loop alist blist slist clist)))

(ripple-carry-adder (list 'a1 'a2 'a3 'a4) ;alist
                    (list 'b1 'b2 'b3 'b4) ;blist
                    (list 's1 's2 's3 's4) ;slist
                    'c) ;c