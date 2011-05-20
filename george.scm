
(define gdev (make-graphics-device #f))

(define (draw-line x1 y1 x2 y2)
  (graphics-draw-line gdev x1 y1 x2 y2))

(graphics-clear gdev)

(define (george) 
  (draw-line .25 0 .35 .5)
  (draw-line .35 .5 .3 .6)
  (draw-line .3 .6 .15 .4)
  (draw-line .15 .4 0 .65)

  (draw-line .4 0 .5 .3)
  (draw-line .5 .3 .6 0)

  (draw-line .75 0 .6 .45)
  (draw-line .6 .45 1 .15)

  (draw-line 1 .35 .75 .65)
  (draw-line .75 .65 .6 .65)
  (draw-line .6 .65 .65 .85)
  (draw-line .65 .85 .6 1)

  (draw-line .4 1 .35 .85)
  (draw-line .35 .85 .4 .65)
  (draw-line .4 .65 .3 .65)
  (draw-line .3 .65 .15 .6)
  (draw-line .15 .6 0 .85))

