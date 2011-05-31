; Example from Lecture 11 on Advanced Data Types (iTutor 6.001)
(define nil '())

(define (find-assoc key alist)
  (cond
   ((null? alist) #f)
   ((equal? key (caar alist)) (cadar alist))
   (else (find-assoc key (cdr alist)))))

(define (add-assoc key val alist)
  (cons (list key val) alist))

(define t2-tag 'table2)
(define (make-table2 size hashfunc)
  (let ((buckets (make-vector size nil)))
    (list t2-tag size hashfunc buckets)))
(define (size-of tbl) (cadr tbl))
(define (hashfunc-of tbl) (caddr tbl))
(define (buckets-of tbl) (cadddr tbl))

(define (table2-get tbl key)
  (let ((index ((hashfunc-of tbl) key (size-of tbl))))
    (find-assoc key
                (vector-ref (buckets-of tbl) index))))

(define (table2-put! tbl key val)
  (let ((index ((hashfunc-of tbl) key (size-of tbl)))
        (buckets (buckets-of tbl)))
    (vector-set! buckets
                 index
                 (add-assoc key
                            val
                            (vector-ref buckets index)))))

; Test case

(define (make-point x y)
  (list 'point x y))
(define (x-point p) (cadr p))
(define (y-point p) (caddr p))

(define (hash-a-point  p n)
  (modulo (+ (x-point p) (y-point p)) n))

(define tt2 (make-table2 4 hash-a-point))
(table2-put! tt2 (make-point 5 5) 20)
(table2-put! tt2 (make-point 5 7) 15)
(table2-get tt2 (make-point 5 5))