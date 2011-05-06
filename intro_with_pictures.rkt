#lang slideshow

(define c (circle 10))

(define r (rectangle 10 20))

(define (square n)
    (filled-rectangle n n))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(define (series mk)
  (hc-append 4 (mk 5 ) (mk 10) (mk 20)))

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "cyan" "blue" "purple")))

(require slideshow/flash)
(require slideshow/code)

(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))

; Uncomment to test procedures...

;(hc-append c r)
;(hc-append 20 c r c)
;(square 10)
;(four (circle 10))
;(checker (colorize (square 10) "red")
;         (colorize (square 10) "black"))
;(checkerboard (square 10))
;(series circle)
;(series square)
;(series (lambda (size) (checkerboard (square size))))
;(rgb-series circle)
;(rgb-series square)
;(series (rgb-maker circle))
;(series (rgb-maker square))
;(list "red" "green" "blue")
;(list (circle 10) (square 10))
;(rainbow (square 5))
;(rainbow (circle 10))
;(apply vc-append (rainbow (filled-rectangle 100 10)))

;(filled-flash 40 30)
;(code (circle 10))
;(pict+code (circle 10))

(require racket/class
         racket/gui/base)

(define f (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas%
         [parent f]
         [style '(border)]
         [paint-callback (lambda (self dc)
                           (drawer dc 0 0))])))
         
;(send f show #t)
;(add-drawing (pict+code (circle 10)))
;(add-drawing (colorize (filled-flash 50 30) "yellow"))
  