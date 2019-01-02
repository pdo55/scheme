(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (cond ((null? rests) nil)
        (else (cons (cons first (car rests)) (cons-all first (cdr rests))))
    )
  )

(define (zip pairs)
  (if (null? pairs) (list nil nil))
  (define (cons-first pairs)
    (if (null? pairs) nil
        (cons (caar pairs) (cons-first (cdr pairs))))
    )
  (define (cons-sec pairs)
    (if (null? pairs) nil
        (cons (car (cdar pairs)) (cons-sec (cdr pairs))))
    )
  (cons (cons-first pairs) (cons (cons-sec pairs) nil))
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17 
  (define (result s count)
    (if (null? s) nil
        (cons (list count (car s)) (result (cdr s) (+ 1 count))))
    )
  (result s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (define (helper lst)
    (cond ((null? lst) nil)
          ((= 0 total) (list nil))
          ((<= (car lst) total) (append (cons-all (car lst) (list-change (- total (car lst)) lst)) (helper (cdr lst))))
          (else (helper (cdr lst)))
      )
    )
  (helper denoms)
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
            (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (begin 
            (define pairs-vals (zip values))
            (define vals (map let-to-lambda (cdr pairs-vals)))
            (define body2 (map let-to-lambda body))
            (cons (list 'lambda (car pairs-vals) (car body2)) (car vals)))
           ))       
            ; END PROBLEM 19
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
