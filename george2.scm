; --------------------------------------------------------------------
; Defined the constructor and selector in terms of primitive procedures

; A vector object, composed of xcor and ycor
(define make-vect list)
(define xcor car)
(define ycor cadr)

; A segment object, composed of start- and end-segment
(define make-segment list)
(define start-segment car)
(define end-segment cadr)

; A rectangle object, composed of a
; origin vector, horizontal and vertical size
(define make-rectangle list)
(define origin car)
(define horiz cadr)
(define vert caddr)

; --------------------------------------------------------------------
; Auxiliary procedures

(define (repeated proc n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x)
	(proc ((repeated proc (- n 1)) x)))))

; ---------------------------------------------------------------------
; Operations on primitives

; Add two vectors producing a vector as result
(define (+vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
	     (+ (ycor v1) (ycor v2))))

; Scales a vector by a given factor
(define (scale-vect vect factor)
  (make-vect (* factor (xcor vect))
	     (* factor (ycor vect))))

; Subtract 2 vectors
(define (-vect v1 v2)
  (+vect v1 (scale-vect v2 -1)))

; Rotate a vector by a given angle
(define (rotate-vect v angle)
  (let ((c (cos angle))
	(s (sin angle)))
    (make-vect (- (* c (xcor v))
		  (* s (ycor v)))
	       (+ (* c (ycor v))
		  (* s (xcor v))))))

; --------------------------------------------------------------------
; Operations on pictures

; Create a procedure to draw a picture inside a rect from a list of segments
(define (make-picture seglist)
  (lambda (rect)
    (for-each (lambda (segment)
		(draw-segment-in-rect rect segment))
	      seglist)))

; Rotate an entire picture by just manipulating it's rect
(define (rotate90 pict)
  (lambda (rect)
    (pict (make-rectangle
	   (+vect (origin rect)
		  (horiz rect))
	   (vert rect)
	   (scale-vect (horiz rect) -1)))))

(define (rotate180 pict)
  (rotate90 (rotate90 pict)))

(define (rotate270 pict)
  (rotate180 (rotate90 pict)))

; Procedure for mapping any point to a given rect
(define (coord-map rect)
  (lambda (point)
    (+vect (+vect (scale-vect (horiz rect)
			      (xcor point))
		  (scale-vect (vert rect)
			      (ycor point)))
	   (origin rect))))

(define (draw-segment-in-rect rect segment)
  (drawline ((coord-map rect) (start-segment segment))
	    ((coord-map rect) (end-segment segment))))

(define (beside p1 p2 a)
  (lambda (rect)
    (p1 (make-rectangle (origin rect)
			(scale-vect (horiz rect) a)
			(vert rect)))
    (p2 (make-rectangle (+vect (origin rect)
			       (scale-vect (horiz rect) a))
			(scale-vect (horiz rect) (- 1 a))
			(vert rect)))))

(define (above p1 p2 a)
  (lambda (rect)
    (p2 (make-rectangle (origin rect)
			(horiz rect)
			(scale-vect (vert rect) (- 1 a))))
    (p1 (make-rectangle (+vect (origin rect)
			       (scale-vect (vert rect) (- 1 a)))
			(horiz rect)
			(scale-vect (vert rect) a)))))

(define (flip-horiz pict)
  (lambda (rect)
    (pict (make-rectangle
	   (+vect (origin rect) (horiz rect))
	   (scale-vect (horiz rect) -1)
	   (vert rect)))))

(define (flip-vert pict)
  (lambda (rect)
    (pict (make-rectangle
	   (+vect (origin rect) (vert rect))
	   (horiz rect)
	   (scale-vect (vert rect) -1)))))

(define (up-push pict n)
  (if (= n 0)
      pict
      (above (up-push pict (- n 1))
	     pict
	     .25)))

(define (right-push pict n)
  (if (= n 0)
      pict
      (beside pict
	      (right-push pict (- n 1))
	      .75)))

(define (corner-push pict n)
  (if (= n 0)
      pict
      (above (beside (up-push pict n)
		     (corner-push pict (- n 1))
		     .75)
	     (beside pict
		     (right-push pict (- n 1))
		     .75)
	     .25)))

(define (4pict p1 r1 p2 r2 p3 r3 p4 r4)
  (beside (above ((repeated rotate90 r1) p1)
		 ((repeated rotate90 r2) p2)
		 .5)
	  (above ((repeated rotate90 r3) p3)
		 ((repeated rotate90 r4) p4)
		 .5)
	  .5))

(define (4same p r1 r2 r3 r4)
  (4pict p r1 p r2 p r3 p r4))

(define (square-limit pict n)
  (4same (corner-push pict n) 1 2 0 3))

; -----------------
; Procedures exclusive from the book SICP

(define (flipped-pairs painter)
  (let ((painter2 (beside painter
			  (flip-vert painter)
			  0.5)))
    (above painter2 painter2 0.5)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (above smaller smaller 0.5) 0.5))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(above (beside smaller smaller 0.5) painter 0.5))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up 0.5))
	      (bottom-right (above right right 0.5))
	      (corner (corner-split painter (- n 1))))
	  (beside (above top-left painter 0.5)
		  (above corner bottom-right 0.5) 0.5)))))

; square-limit as defined by the SICP book
(define (sicp-square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter 0.5)))
      (above half (flip-vert half) 0.5))))

; ---------------------------------------------------------------------
; Picture definitions

; Define the points of george, used in constructing the line segments
(define p1 (make-vect .25 0))
(define p2 (make-vect .35 .5))
(define p3 (make-vect .3 .6))
(define p4 (make-vect .15 .4))
(define p5 (make-vect 0 .65))

(define p6 (make-vect .4 0))
(define p7 (make-vect .5 .3))
(define p8 (make-vect .6 0))

(define p9 (make-vect .75 0))
(define p10 (make-vect .6 .45))
(define p11 (make-vect 1 .15))

(define p12 (make-vect 1 .35))
(define p13 (make-vect .75 .65))
(define p14 (make-vect .6 .65))
(define p15 (make-vect .65 .85))
(define p16 (make-vect .6 1))

(define p17 (make-vect .4 1))
(define p18 (make-vect .35 .85))
(define p19 (make-vect .4 .65))
(define p20 (make-vect .3 .65))
(define p21 (make-vect .15 .6))
(define p22 (make-vect 0 .85))

; Describe george as a list of line segments
(define george-lines
  (list (make-segment p1 p2)
	(make-segment p2 p3)
	(make-segment p3 p4)
	(make-segment p4 p5)

	(make-segment p6 p7)
	(make-segment p7 p8)

	(make-segment p9 p10)
	(make-segment p10 p11)

	(make-segment p12 p13)
	(make-segment p13 p14)
	(make-segment p14 p15)
	(make-segment p15 p16)

	(make-segment p17 p18)
	(make-segment p18 p19)
	(make-segment p19 p20)
	(make-segment p20 p21)
	(make-segment p21 p22)))

(define g (make-picture george-lines))

(define letter-a
  (make-picture
   (list
    (make-segment (make-vect 0.5 0.9) (make-vect 0.2 0.1))
    (make-segment (make-vect 0.5 0.9) (make-vect 0.8 0.1))
    (make-segment (make-vect 0.35 0.5) (make-vect 0.65 0.5)))))

(define letter-b
  (make-picture
   (list
    (make-segment (make-vect 0.25  0.9) (make-vect 0.25  0.1))
    (make-segment (make-vect 0.25  0.5) (make-vect 0.625 0.5))
    (make-segment (make-vect 0.25  0.9) (make-vect 0.625 0.9))
    (make-segment (make-vect 0.25  0.1) (make-vect 0.625 0.1))
    (make-segment (make-vect 0.625 0.9) (make-vect 0.75  0.7))
    (make-segment (make-vect 0.625 0.5) (make-vect 0.75  0.7))
    (make-segment (make-vect 0.625 0.1) (make-vect 0.75  0.3))
    (make-segment (make-vect 0.625 0.5) (make-vect 0.75  0.3)))))

(define letter-h
  (make-picture
   (list
    (make-segment (make-vect 0.3 0.9) (make-vect 0.3 0.1))
    (make-segment (make-vect 0.7 0.9) (make-vect 0.7 0.1))
    (make-segment (make-vect 0.3 0.5) (make-vect 0.7 0.5)))))

(define empty-picture (make-picture ()))

(define big-bro
  (beside g
	 (above empty-picture g .5)
	 .5))

(define acrobats
  (beside g
	  (rotate180 (flip-horiz g))
	  .5))

(define 4bats
  (above acrobats
	 (flip-horiz acrobats)
	 .5))

; -----------------------------------------------------------------------------
; Some procedures to actually draw something on the graphics window

(define (drawline v1 v2)
  (graphics-draw-line gdev (xcor v1) (ycor v1) (xcor v2) (ycor v2)))

(define (draw-segment s)
  (drawline (start-segment s) (end-segment s)))

; ----------------------------------------------------------------------------
; Tests

; =====> Uncomment to initialize graphics <=====
;(define gdev (graphics-make-device #f))
;(for-each draw-segment george-lines)

; Defining a rectangle that will fill the default screen coordinates
(define gdev-rect (make-rectangle (make-vect -1 -1)
				  (make-vect 2 0)
				  (make-vect 0 2)))

(define test-rect (make-rectangle (make-vect -0.3 -0.7)
				  (make-vect 0.5 0)
				  (make-vect 0.5 1)))

(graphics-clear gdev)
;(g gdev-rect)
;(letter-a test-rect)
;(letter-b gdev-rect)
;(letter-h gdev-rect)

;((beside (beside letter-a letter-b 0.25)
;	 (beside g (rotate90 g) 0.75) 0.5)
; gdev-rect)

;(big-bro gdev-rect)

;(4bats gdev-rect)

;((up-push g 2) gdev-rect)
;((right-push g 2) gdev-rect)
;((corner-push 4bats 3) gdev-rect)

;((4same g 0 1 2 3) gdev-rect)

;((square-limit g 2) gdev-rect)
;((square-limit (4pict letter-a 0
;		      letter-b 0
;		      letter-h 0
;		      4bats 0)
;	       2) gdev-rect)

;(define wave4 (flipped-pairs g))
;(wave4 gdev-rect)
;((corner-split g 5) gdev-rect)

;((sicp-square-limit (beside letter-h (beside letter-b letter-a 0.5) 0.333) 3) gdev-rect)
      
      
(graphics-clear gdev)

