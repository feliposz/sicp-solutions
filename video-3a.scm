; BUG!!!
; The way horiz and vert is implemented is different from what is intended

; Defined the constructor and selector in terms of primitive procedures
(define make-vector cons)
(define x-cord car)
(define y-cord cdr)

(define make-segment cons)
(define start-seg car)
(define end-seg cdr)

; Example of adding vectors
(define (+vector v1 v2)
  (make-vector (+ (x-cord v1) (x-cord v2))
	       (+ (y-cord v1) (y-cord v2))))

; Example of scaling a vector
(define (scale-vector s v)
  (make-vector (* s (x-cord v))
	       (* s (y-cord v))))

; Constructing a more complex structure
; Segment (a pair of pairs)
(make-segment (make-vector 2 3)
	      (make-vector 5 1))

; A rectangle object, composed of a
; origin vector, horizontal and vertical size

(define (make-rect origin horiz vert)
  (list origin horiz vert))

(define (rect-orig rect)
  (car rect))

(define (rect-horiz rect)
  (cadr rect))

(define (rect-vert rect)
  (caddr rect))

; Procedure for mapping any point to a given rect
(define (coord-map rect)
  (lambda (point)
    (+vector (+vector (* (rect-horiz rect)
			 (x-cord point))
		      (* (rect-vert rect)
			 (y-cord point)))
	     (rect-orig rect))))

; Creates a procedure to draw a certain picture given a rect to draw in
(define (make-picture seglist)
  (lambda (rect)
    (for-each (lambda (s)
		(drawline ((coord-map rect) (start-seg s))
			  ((coord-map rect) (end-seg s))))
	      seglist)))

(define (beside p1 p2 a)
  (lambda (rect)
    (p1 (make-rect (rect-orig rect)
		   (* a (rect-horiz rect))
		   (rect-vert rect)))
    (p2 (make-rect (+vector (rect-orig rect)
			    (make-vector (* a (rect-horiz rect) 0)))
		   (* (- 1 a) (rect-horiz rect))
		   (rect-vert rect)))))

(define (drawline v1 v2)
  (graphics-draw-line gdev (x-cord v1) (y-cord v1) (x-cord v2) (y-cord v2)))

(define letter-a
  (make-picture
   (list
    (make-segment (make-vector  0.0  0.8) (make-vector -0.5  -0.8))
    (make-segment (make-vector  0.0  0.8) (make-vector  0.5  -0.8))
    (make-segment (make-vector -0.25 0.0) (make-vector  0.25  0.0)))))

(define myrect (make-rect (make-vector -0.7 -0.7) 1.4 1.4))

((beside letter-a letter-a 0.5) myrect)



;(define gdev (make-graphics-device #f))
;(graphics-clear gdev)
(define (letter-a)
  (graphics-draw-line gdev 0 0.8 -0.5 -0.8)
  (graphics-draw-line gdev 0 0.8 0.5 -0.8)
  (graphics-draw-line gdev -0.25 0 0.25 0))

(define (letter-b)
  (graphics-draw-line gdev -0.5 0.8 -0.5 -0.8)
  (graphics-draw-line gdev -0.5 0.0 0.25 0.0)
  (graphics-draw-line gdev -0.5 0.8 0.25 0.8)
  (graphics-draw-line gdev -0.5 -0.8 0.25 -0.8)
  (graphics-draw-line gdev 0.25 0.8 0.5 0.4)
  (graphics-draw-line gdev 0.25 0 0.5 0.4)
  (graphics-draw-line gdev 0.25 -0.8 0.5 -0.4)
  (graphics-draw-line gdev 0.25 0 0.5 -0.4))

;(letter-b)





