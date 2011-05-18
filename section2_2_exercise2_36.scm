(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))       ; Get first element of all seqs
	    (accumulate-n op init (map cdr seqs)))))  ; Remove 1st from all seqs

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(accumulate-n + 0 (list (list  1  2  3)
			(list  4  5  6)
			(list  7  8  9)
			(list 10 11 12)))
