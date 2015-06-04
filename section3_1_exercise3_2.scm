(define (make-monitored f)
  (let ((counter 0))
    (lambda (param)
      (cond ((eq? param 'how-many-calls?)             
             counter)
            ((eq? param 'reset-count)
             (set! counter 0)
             counter)
            (else 
             (set! counter (+ counter 1))
             (f param))))))

(define s (make-monitored sqrt))

(s 2)
(s 3)
(s 4)

(s 'how-many-calls?)
(s 'reset-count)

(s 2)
(s 3)
(s 'how-many-calls?)
