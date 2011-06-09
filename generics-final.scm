; This program requires the procedures defined in examples/ch2support.scm
(load "examples\\ch2support.scm")

; Apply generic with raise

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2) ; Should have applied before
                    (error "No method for these types -- APPLY-GENERIC"
                           (list op type-tags))
                    (if (higher-type? type1 type2)
                        (apply-generic op a1 (raise a2))
                        (apply-generic op (raise a1) a2))))
              (error "No method for these arguments -- APPLY-GENERIC"
                     (list op type-tags)))))))

; Tag operations

(define (attach-tag type-tag contents)
  (if (or (eq? type-tag 'integer) (eq? type-tag 'real))
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((integer? datum) 'integer)
        ((real? datum) 'real)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (if (number? datum)
      datum
      (if (pair? datum)
          (cdr datum)
          (error "Bad tagged datum -- CONTENTS" datum))))

; Integer numbers package
; TODO: force only integer numbers on creation?

(define (install-integer-package)
  (define (integer->rational n)
    (make-rational n 1))
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))  
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? 'integer
       (lambda (x) (= x 0)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'raise '(integer)
       integer->rational)
  
  (put 'cos '(integer) (lambda (x) (make-real (cos x))))
  (put 'sin '(integer) (lambda (x) (make-real (sin x))))
  (put 'sqrt '(integer) (lambda (x) (make-real (sqrt x))))
  (put 'square '(integer) (lambda (x) (make-real (square x))))  
  (put 'atan '(integer integer) (lambda (y x) (make-real (atan y x))))  
  
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

; Real numbers package

(define (install-real-package)
  (define (real->complex n)
    (make-from-real-imag (make-real n) (make-real 0)))
  (define (real->rational n)
    (make-rational (round (* n 1000)) 1000))
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))  
  (put 'equ? '(real real)
       (lambda (x y) (=  x  y)))
  (put '=zero? 'real
       (lambda (x) (= x 0.0)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real)
       real->complex)
  (put 'project '(real)
       real->rational)

  (put 'cos '(real) (lambda (x) (tag (cos x))))
  (put 'sin '(real) (lambda (x) (tag (sin x))))
  (put 'atan '(real real) (lambda (y x) (tag (atan y x))))
  (put 'sqrt '(real) (lambda (x) (tag (sqrt x))))
  (put 'square '(real) (lambda (x) (tag (square x))))

  'done)

(define (make-real n)
  ((get 'make 'real) n))

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
      (normalize (/ n g) ; CHECK: not sure if this is ok
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
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (rational->real n)
    (make-real (* 1.0 (/ (numer n)
                         (denom n)))))
  (define (rational->integer n)
    (make-integer (numer n)))
  
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  (put '=zero? 'rational
       (lambda (x) (= (numer x) 0)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       rational->real)
  (put 'project '(rational)
       rational->integer)
  
  (put 'cos '(rational) (lambda (r) (make-real (cos (/ (numer r) (denom r) 1.0)))))
  (put 'sin '(rational) (lambda (r) (make-real (sin (/ (numer r) (denom r) 1.0)))))
  (put 'sqrt '(rational) (lambda (r) (make-real (sqrt (/ (numer r) (denom r) 1.0)))))
  (put 'square '(rational) (lambda (r) (make-real (square (/ (numer r) (denom r) 1.0)))))  
  
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; Complex numbers

(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (apply-generic 'sqrt (apply-generic 'add
                                        (apply-generic 'square (real-part z))
                                        (apply-generic 'square (imag-part z)))))
  (define (angle z)
    (apply-generic 'atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y)
    (cons x y))
;  (define (make-from-mag-ang r a)
;    (cons (* r (cos a)) (* r (sin a))))

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
;  (put 'make-from-mag-ang
;       'rectangular
;       (lambda (r a)
;	 (tag (make-from-mag-ang r a))))
  'done)


(define (install-polar-package)
  ;;internal procedures
  (define (real-part z) (apply-generic 'mul (magnitude z) (apply-generic 'cos (angle z))))
  (define (imag-part z) (apply-generic 'mul (magnitude z) (apply-generic 'sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  ;(define (make-from-real-imag x y)
  ;  (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (make-from-mag-ang r a) (cons r a))

  ;;interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
;  (put 'make-from-real-imag
;       'polar
;       (lambda (x y) (tag (make-from-real-imag x y))))
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
    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
                         (apply-generic 'add (imag-part z1) (imag-part z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
  
  (define (mul-complex z1 z2)
    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
                       (apply-generic 'add (angle z1) (angle z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
                       (apply-generic 'sub (angle z1) (angle z2))))

  (define (equ-complex? z1 z2)
    (or (and (apply-generic 'equ? (real-part z1) (real-part z2))
             (apply-generic 'equ? (imag-part z1) (imag-part z2)))
        (and (apply-generic 'equ? (magnitude z1) (magnitude z2))
             (apply-generic 'equ? (angle z1) (angle z2)))))
  
  (define (complex->real z)
    (let ((x (real-part z)))
      (cond ((eq? (type-tag x) 'real) x)
            ((eq? (type-tag x) 'rational) (raise x))
            ((eq? (type-tag x) 'integer) (raise (raise x)))
            (else (error ("Conversion from this type not implemented -- COMPLEX->REAL " (type-tag x)))))))
  
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
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ-complex? z1 z2)))
  (put '=zero? 'complex
       (lambda (z) (or (= (magnitude z) 0)
                       (= (real-part z) 0)))) ; CHECK: Is this valid?
  (put 'project '(complex)
       complex->real)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
 
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; Type raising/dropping

(define (raise value)
  (apply-generic 'raise value))

(define (drop value)
  (if (= (type-level (type-tag value)) 0)
      value
      (let ((projected (apply-generic 'project value)))
        (let ((re-raised (raise projected)))
          (if (apply-generic 'equ? value re-raised)
              (drop projected)
              value)))))
  
(define (type-level type)
  (define hierarchy '(integer rational real complex))
  (define (type-search list position)
    (cond ((null? list) (error "Not a valid type -- TYPE-LEVEL " item))
          ((eq? type (car list)) position)
          (else (type-search (cdr list) (+ 1 position)))))
  (type-search hierarchy 0))
        
(define (higher-type? t1 t2)
  (let ((l1 (type-level t1))
        (l2 (type-level t2)))
    (> l1 l2)))

; Generic operations

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))
(define (equ? x y)
  (apply-generic 'equ? x y))
(define (=zero? x)
  ((get '=zero? (type-tag x)) x))

; Install packages

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

