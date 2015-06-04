;TODO: Need to implement this. Have no idea where to start.

(define (queens board-size)
   (define (queen-cols k)  
     (if (= k 0)
         (list empty-board)
         (filter
          (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
   (queen-cols board-size))
 
(define nil ())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
	    (enumerate-interval (+ low 1) high))))

; (queens 8)