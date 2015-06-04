(define L1 (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr L1)))))

(define L2 (list (list 7)))

(car (car L2))

(define L3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr L3))))))))))))
