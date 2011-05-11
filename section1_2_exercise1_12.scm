; Computes elements of the Pascal's triangle recursively

(define (pascal i j)
  (if (or (= j 0) (= j i))
      1
      (+ (pascal (- i 1) j)
	 (pascal (- i 1) (- j 1)))))

(list (pascal 0 0))
(list (pascal 1 0) (pascal 1 1))
(list (pascal 2 0) (pascal 2 1) (pascal 2 2))
(list (pascal 3 0) (pascal 3 1) (pascal 3 2) (pascal 3 3))
(list (pascal 4 0) (pascal 4 1) (pascal 4 2) (pascal 4 3) (pascal 4 4))



