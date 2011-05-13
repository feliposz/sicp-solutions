(define (addr a b)
  (if (= a 0)
      b
      (1+ (addr (-1+ a) b))))

(define (addi a b)
  (if (= a 0)
      b
      (addi (-1+ a) (1+ b))))

;Recursive procedure
(addr 4 5)
;(if (= 4 0) 5 (1+ (add (-1+ 4) 5)))
;(1+ (add 3 5))
;(1+ (if (= 3 0) 5 (1+ (add (-1+ 3) 5))))
;(1+ (1+ (add 2 5)))
;(1+ (1+ (if (= 2 0) 5 (1+ (add (-1+ 2) 5)))))
;(1+ (1+ (1+ (add 1 5))))
;(1+ (1+ (1+ (if (= 1 0) 5 (1+ (add (-1+ 1) 5))))))
;(1+ (1+ (1+ (1+ (add 0 5)))))
;(1+ (1+ (1+ (1+ (if (= 0 0) 5 (1+ (add (-1+ 0) 5)))))))
;(1+ (1+ (1+ (1+ 5))))
;(1+ (1+ (1+ 6)))
;(1+ (1+ 7))
;(1+ 8)
;9

;Iterative procedure
(addi 4 5)
;(if (= 4 0) 5 (add (-1+ 4) (1+ 5)))
;(addi 3 6)
;(if (= 3 0) 6 (add (-1+ 3) (1+ 6)))
;(addi 2 7)
;(if (= 2 0) 7 (add (-1+ 2) (1+ 7)))
;(addi 1 8)
;(if (= 1 0) 8 (add (-1+ 1) (1+ 8)))
;(addi 0 9)
;(if (= 0 0) 9 (add (-1+ 0) (1+ 9)))
;9


