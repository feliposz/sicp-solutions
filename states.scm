(define (1+ x) (+ 1 x))

(define count 1)
(define (demo x)
  (set! count (1+ count))
  (+ x count))

; demo is not a function, since it doesn't return the same value all the time

; Functional version
(define (factf n)
  (define (iter m i) ; m = product
    (cond ((> i n) m)
          (else (iter (* i m) (+ 1 i)))))
  (iter 1 1))

(define (fact n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
            (else
             (set! m (* i m))
             (set! i (+ 1 i))
             (loop))))
    (loop)))

((lambda (y) ((lambda (x) (* x y)) 3)) 4)

(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (+ n 1))
      n)))

(define c1 (make-counter 0))

(define c2 (make-counter 10))

