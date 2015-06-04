; Example from Lecture 11 on Advanced Data Types (iTutor 6.001)
(define (find-assoc key alist)
  (cond
   ((null? alist) #f)
   ((equal? key (caar alist)) (cadar alist))
   (else (find-assoc key (cdr alist)))))

(define (add-assoc key val alist)
  (cons (list key val) alist))

; Test cases

(define a1 '((x 15) (y 20)))
(find-assoc 'y a1)

(define a2 (add-assoc 'y 10 a1))
(find-assoc 'y a2)