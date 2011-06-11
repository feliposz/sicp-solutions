(define (make-table2)
  (list '*table2*))

(define (lookup2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable    
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))  

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert2! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value) (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

; test

(define t2 (make-table2))
(insert2! 'a 1 'apple t2)
(insert2! 'a 2 'apricot t2)
(insert2! 'b 1 'banana t2)
(insert2! 'b 2 'beaf t2)
(insert2! 'c 1 'cherry t2)
(insert2! 'c 2 'chocolate t2)
t2
(lookup2 'a 1 t2)
(lookup2 'c 2 t2)
(insert2! 'a 1 'pineapple t2)
(lookup2 'a 1 t2)
(lookup2 'a 3 t2)
(lookup2 'd 1 t2)
t2
