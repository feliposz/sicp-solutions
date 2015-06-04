; Circuit language from video 5B - Computational Objects

;(define a (make-wire))
;(define b (make-wire))
;(define c (make-wire))
;(define d (make-wire))
;(define e (make-wire))
;(define s (make-wire))
;
;(or-gate a b d)
;(and-gate a b c)
;(inverter c e)
;(and-gate d e s)

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

(define (ripple-carry-adder alist blist slist c)
  (let ((clist (list c ))) ; the last carry will be in the end of the list
    (define (fill-clist n)
      (cond ((= n 0) 'done)
            (else
             (set! clist (cons (make-wire) clist))
             (fill-clist (- n 1)))))                  
    (define (loop alist blist slist clist)
      (cond ((null? alist) 'done)
            (else
             (full-adder (car alist)
                         (car blist)
                         (car clist) ;c-in = current
                         (car slist)
                         (cadr clist)) ; c-out = next
             (loop (cdr alist) (cdr blist) (cdr slist) (cdr clist)))))
    (fill-clist (length alist))
    (loop alist blist slist clist)))

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

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline)
                 )))
    
; wire object

(define (make-wire)
  (let ((signal 0) (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
            (else
             (set! signal new)
             (call-each action-procs))))
    (define (accept-action-proc! proc)
      (set! action-procs
            (cons proc action-procs))
      ;COMMENTED FOR EXERCISE 3.31
      ;(proc) 
      )
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc!)
            (else (error "Unknown operation - WIRE" m))))
    dispatch))

(define (call-each procedures)
  (cond ((null? procedures) 'done)
        (else
         ((car procedures))
         (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

; helper

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (cond ((empty-agenda? the-agenda)
         'done)
        (else
         ((first-agenda-item the-agenda))
         (remove-first-agenda-item! the-agenda)
         (propagate))))

; agenda

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action)
                                    segments))
        (add-to-segments! segments))))
  
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; time-segments

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; queue

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) 
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "Front called with an empty queue")
          (car front-ptr )))
    (define (insert-queue! elt)
      (let ((new-pair (cons elt nil)))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "Delete of empty queue"))
            (else
             (set! front-ptr (cdr front-ptr )))))
    (define (print-queue)
      (if (empty-queue?)
          (display "The queue is empty")
          (begin
            (display "Queue:")
            (display front-ptr)))
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Unknown method for queue:" m))))
    dispatch))

(define (empty-queue? q) ((q 'empty-queue?)))
(define (front-queue q) ((q 'front-queue)))
(define (insert-queue! q elt) ((q 'insert-queue!) elt))
(define (delete-queue! q) ((q 'delete-queue!)))
(define (print-queue q) ((q 'print-queue)))

; global definitions

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; examples / test cases

(newline)
(display "TEST: half-adder") (newline)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
  
(probe 'sum sum) ; SUM 0 NEW-VALUE = 0
  
(probe 'carry carry) ; CARRY 0 NEW-VALUE = 0

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

(propagate)
; SUM 8 NEW-VALUE = 1  <<< never gets executed
; DONE

(set-signal! input-2 1)
(propagate)
; CARRY 11 NEW-VALUE = 1
; SUM 16 NEW-VALUE = 0  <<< never gets executed
; DONE
