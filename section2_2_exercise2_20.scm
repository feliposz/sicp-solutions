; Return only elements of l that pass the test (f)
(define (filter-list f l)
  (if (null? l)
      nil
      (if (f (car l))
	  (cons (car l) (filter-list f (cdr l)))
	  (filter-list f (cdr l)))))

; Return a list of elements whose parity is the same as the first
(define (same-parity first . rest)
  (if (even? first)
      (cons first (filter-list even? rest))
      (cons first (filter-list odd? rest))))

(same-parity 2 3 4 5 6 7 8 9 10 11 12 13)

