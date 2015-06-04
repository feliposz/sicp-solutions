(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (a-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (a-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (a-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(a-length (list "a" (list 1 2) "b"))

(a-append (list 1 2 3) (list 4 5 6))

(a-map square (list 2 3 4 5))