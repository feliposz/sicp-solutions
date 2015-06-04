; s = sum, c = carry
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))
    
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

; primitives

(define (inverter in out)
  (define (inverter-in)
    (let ((new
           (logical-not (get-signal in))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! in inverter-in))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (warng "invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((= s1 0) 0)
        ((= s2 0) 0)
        (else
         (warng "invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                        (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((= s1 1) 1)
        ((= s2 1) 1)
        (else
         (warng "invalid signal" s1 s2))))
