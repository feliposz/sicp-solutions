(define nil '())

(define (make-queue) (cons 'queue (cons nil nil)))

(define (queue? q)
  (and (pair? q) (eq? 'queue (car q))))

(define (front-ptr q) (cadr q))
(define (rear-ptr q) (cddr q))

(define (set-front-ptr! q item)
  (set-car! (cdr q) item))

(define (set-rear-ptr! q item)
  (set-cdr! (cdr q) item))

(define (empty-queue? q) 
  (if (not (queue? q))
      (error "Object not a queue:" q)
      (null? (front-ptr q))))

(define (front-queue q)
  (if (empty-queue? q)
      (error "Front called with an empty queue:" q)
      (car (front-ptr q))))

(define (insert-queue! q elt)
  (let ((new-pair (cons elt nil)))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "Delete of empty queue:" q))
        (else
         (set-front-ptr! q (cdr (front-ptr q)))
         ; Not really necessary, but keeps data cleaner
         (if (empty-queue? q) (set-rear-ptr! q nil)) 
         q)))

(define (print-queue q)
  (if (empty-queue? q)
      (display "The queue is empty")
      (begin
        (display "Queue:")
        (display (front-queue q))))
  (newline))
      

; Test cases

(define q (make-queue))
(empty-queue? q)
(insert-queue! q 'b)
(insert-queue! q 'c)
(insert-queue! q 'd)
(front-queue q)
(insert-queue! q 'a)
(delete-queue! q)
(front-queue q)
(delete-queue! q)
(front-queue q)
(delete-queue! q)
(front-queue q)
(delete-queue! q)
(insert-queue! q 'e)