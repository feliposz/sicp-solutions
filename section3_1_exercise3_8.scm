(define f
  (let ((x2 -1))
    (lambda (x)
        (let ((previous x2))
          (set! x2 x)
          (if (and (= previous 1) (= x 0))
              1
              0)))))

; Could have been a better implementation =/
(+ (f 0) (f 1))
