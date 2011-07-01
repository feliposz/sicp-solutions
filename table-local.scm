(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))      
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (print) ; print in the same order that items where inserted
      (define (print-values values)
        (cond ((null? values) 'done)
              (else
               (print-values (cdr values))
               (display (car values))
               (display " ")
               )))
      (define (print-lines lines)
        (cond ((null? lines) 'done)
              (else
               (print-lines (cdr lines))
               (display (caar lines)) (display ": ") ; header
               (print-values (cdar lines)) ; items
               (newline)
               )))
      (print-lines (cdr local-table)))
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

;(put 'a 1 'apple)
;(put 'a 2 'apricot)
;(put 'b 1 'banana)
;(put 'b 2 'beaf)
;(put 'c 1 'cherry)
;(put 'c 2 'chocolate)
;(print-table)
;(get 'a 1)
;(get 'c 2)
;(put 'a 1 'pineapple)
;(get 'a 1)
;(get 'a 3)
;(get 'd 1)
;(print-table)
