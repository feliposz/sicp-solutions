(define random-init 13061981)

; From K&R - The C Programming Language
(define rand-update
  (lambda (x)
    (modulo (quotient (+ (* 1103515245 x)
                         12345)
                      65536)
            32768)))

(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (set! x (rand-update x))
             x)
            ((eq? op 'reset)
             (lambda (new)
               (set! x new)))
            (else
             (error "Unknown operator -- RAND " operator))))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 13061981)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)