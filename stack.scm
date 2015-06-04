(define (make-stack) (cons 'stack '()))

(define (stack? stack)
  (and (pair? stack) (eq? 'stack (car stack))))

(define (empty-stack? stack)
  (if (not (stack? stack))
      (error "Object not a stack:" stack)
      (null? (cdr stack))))

(define (insert! stack elt)
  (cond ((not (stack? stack))
         (error "Object not a stack:" stack))
        (else
         (set-cdr! stack (cons elt (cdr stack)))
         stack)))

(define (top stack)
  (if (empty-stack? stack)
      (error "Stack underflow -- TOP")
      (cadr stack)))

(define (delete! stack)
  (if (empty-stack? stack)
      (error "Stack underflow -- DELETE")
      (set-cdr! stack (cddr stack)))
  stack)

; Test cases

(define s (make-stack))

(insert! s 'a)

(insert! s 'b)
