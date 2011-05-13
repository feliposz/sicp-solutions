(define (make-pair a b)
  (cons a b))

(define (a-pair p)
  (car p))

(define (b-pair p)
  (cdr p))

(define (value-pair p)
  (* (expt 2 (a-pair p)) (expt 3 (b-pair p))))