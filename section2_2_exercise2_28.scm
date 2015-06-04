(define (fringe tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))
	

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))

(define animals (list "tiger" "wolf" "eagle"))
(define vegetables (list "tree" "grass" "flower"))
(define metals (list "iron" "gold" "silver"))
(define minerals (list "granite" "diamond" "coal"))
(define organics (list animals vegetables))
(define inorganics (list metals minerals))
(define things (list organics inorganics))
(fringe things)

