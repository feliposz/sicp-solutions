; Product of 2 vectors [ A B C ] [ D E F] = [AD BE CF]
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Product of matrix and vector [ A B ] . [ X ] = [ AX BY ]
;                              [ C D ]   [ Y ]   [ CX DY ]
(define (matrix-*-vector m v)
  (map (lambda (l)
	 (dot-product l v))
       m))

; Transpose matrix [ A B ] => [ A C ]
;                  [ C D ]    [ B D ]
(define (transpose mat)
  (accumulate-n cons nil mat))

; Product of matrices [ A B ] . [ X Y ] = [ A*X+B*W A*Y+B*Z ]
;                     [ C D ]   [ W Z ]   [ C*X+D*W C*Y+D*Z ]
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (line)
	   (matrix-*-vector cols line))
	 m)))

; TEST CASES

(define m1 (list (list 1 2 3 4)
		 (list 4 5 6 6)
		 (list 6 7 8 9)))

(define id (list (list 1 0 0 0)
		 (list 0 1 0 0)
		 (list 0 0 1 0)))

(dot-product (list 1 2 3) (list 1 2 3)) ;=> 14

(transpose m1) ;=> ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(matrix-*-vector m1 (list 1000 100 10 1)) ;=> (1234 4566 6789)

(matrix-*-matrix id m1) ;=> ((1 2 3 4) (4 5 6 6) (6 7 8 9))
(matrix-*-matrix (transpose m1) (transpose id)) ;=> ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(matrix-*-matrix (list (list 14 9 3)
		       (list 2 11 15)
		       (list 0 12 17)
		       (list 5 2 3))
		 (list (list 12 25)
		       (list 9 10)
		       (list 8 5))) ;=> ((273 455) (243 235) (244 205) (102 160))
(define mA (list (list 1 0)
		 (list 0 2)))
(define mB (list (list 0 1)
		 (list 1 0)))
(matrix-*-matrix mA mB) ;=> ((0 1) (2 0))
(matrix-*-matrix mB mA) ;=> ((0 2) (1 0))



; Standard definitions for accumulate

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


