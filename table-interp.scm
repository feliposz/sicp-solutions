; Example from Lecture 11 on Advanced Data Types (iTutor 6.001)
(define nil '())

(define (find-assoc key alist)
  (cond
   ((null? alist) #f)
   ((equal? key (caar alist)) (cadar alist))
   (else (find-assoc key (cdr alist)))))

(define (add-assoc key val alist)
  (cons (list key val) alist))

(define table-tag 'table)

(define (make-table) (cons table-tag nil))

(define (table-get tbl key)
  (find-assoc key (cdr tbl)))

(define (table-put! tbl key val)
  (set-cdr! tbl (add-assoc key val (cdr tbl))))

; Test case

;(define tt1 (make-table1))
;(table1-put! tt1 'y 20)
;(table1-put! tt1 'x 15)