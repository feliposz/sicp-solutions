(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
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
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))


(define (close-enough? x y)
  (if (and (number? x) (number? y))
      (< (abs (- x y)) 0.01)
      (equal? x y)))
(define aprox-table (make-table close-enough?))
(define get-aprox (aprox-table 'lookup-proc))
(define put-aprox (aprox-table 'insert-proc))


; test

(put-aprox 'real 3.1415926 'pi)
(put-aprox 'integer 3 'three)
(put-aprox 'integer 0 'zero)
(put-aprox 'foo 'bar 'foobar)

(get-aprox 'real 3.1416)
(get-aprox 'integer 2.9999)
(get-aprox 'integer 0.0001)
(get-aprox 'foo 'bar)

(get-aprox 'real 3.1)
(get-aprox 'integer 2.9)
(get-aprox 'integer 0.1)
(get-aprox 'integer 12345)
(get-aprox 'bar 'foo)
