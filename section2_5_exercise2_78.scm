; This program requires the procedures defined in examples/ch2support.scm
(load "examples\\ch2support.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

; Tag operations

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (if (pair? datum)
          (car datum)
          (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (if (number? datum)
      datum
      (if (pair? datum)
          (cdr datum)
          (error "Bad tagged datum -- CONTENTS" datum))))

; Conventional numbers package

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))  
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; Rational

(define (install-rational-package)
  (define (numer r) (car r))
  (define (denom r) (cadr r))
  (define (make-rat n d)
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
    (define (normalize n d)
      (if (and (> n 0) (< d 0))
          (list (- n) (- d))
          (list n d)))
    (let ((g (gcd n d)))
      (normalize (/ n g)
                 (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))  
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; Complex numbers

(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;;interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag
       'rectangular
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang
       'rectangular
       (lambda (r a)
	 (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  ;;internal procedures
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))

  ;;interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  (put 'make-from-real-imag
       'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang
       'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ; Exercise 2.77:
  ; 1)Adding theses procedures will make the procedure magnitude
  ; apply the generic operation defined for the tagged object
  ; complex, stripping it's tag and recalling the same procedure
  ; but with a rectangular (or polar) tagged object. Then, the
  ; appropriate internal procedure is called.
  ; 2)Apply-generic is invoked 2 times
  ; 3)The first time, the global procedure magnitude is called
  ; with a complex-tagged object.
  ; Then, the global magnitude is called again with a
  ; polar/rectangular-tagged object.
  ; Third, the local polar/rect magnitude procedure is called.
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Install packages

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Test cases

(apply-generic 'add (make-scheme-number 3) (make-scheme-number 4))
(apply-generic 'sub (make-scheme-number 3) (make-scheme-number 4))
(apply-generic 'mul (make-scheme-number 3) (make-scheme-number 4))
(apply-generic 'div (make-scheme-number 3) (make-scheme-number 4))

(apply-generic 'add 3 4)
(apply-generic 'sub 3 4)
(apply-generic 'mul 3 4)
(apply-generic 'div 3 4)

(apply-generic 'add (make-rational 1 2) (make-rational 3 4))
(apply-generic 'sub (make-rational 1 2) (make-rational 3 4))
(apply-generic 'mul (make-rational 1 2) (make-rational 3 4))
(apply-generic 'div (make-rational 1 2) (make-rational 3 4))

(apply-generic 'add (make-from-real-imag 1 2) (make-from-mag-ang 3 4))
(apply-generic 'sub (make-from-real-imag 1 2) (make-from-mag-ang 3 4))
(apply-generic 'mul (make-from-real-imag 1 2) (make-from-mag-ang 3 4))
(apply-generic 'div (make-from-real-imag 1 2) (make-from-mag-ang 3 4))

(magnitude (make-from-real-imag 3 4))