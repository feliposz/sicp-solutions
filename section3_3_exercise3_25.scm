; This is an actual 1-dimension table, that is using a list as a key
; to emulate multi-dimensional tables

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (print) ; print in the same order that items where inserted
      (define (print-values values)
        (cond ((null? values) 'done)
              (else
               (print-values (cdr values))
               (display (car values))
               (newline)
               )))
      (print-values (cdr local-table)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))
(define print-table (operation-table 'print))


; test

(put '(a 1) 'apple)
(put '(a 2) 'apricot)
(put '(b 1) 'banana)
(put '(b 2) 'beaf)
(put '(c 1 1) 'cherry)
(put '(c 2 1) 'chocolate)
(put '(pi) 3.1415926)

(print-table)
(get '(a 1))
(get '(c 2 1))
(put '(a 1) 'pineapple)
(get '(a 1))
(get '(a 3))
(get '(d 1))
(get '(pi))
(print-table)
