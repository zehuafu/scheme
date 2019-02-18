(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(define (cons-all first rests)
  (if (null? rests)
      nil
      (cons (cons first (car rests)) (cons-all first (cdr rests)))
  ))

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))

;; Returns a list of two-element lists
(define (enumerate s)
  (define (helper i s)
          (if (null? s)
              nil
              (cons (cons i (cons (car s) nil)) (helper (+ i 1) (cdr s)))))
  (helper 0 s))

;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  (cond
    ((or (null? denoms) (= total 0)) nil)
    ((= total (car denoms)) (cons (cons (car denoms) nil) (list-change total (cdr denoms))))
    ((< total (car denoms)) (list-change total (cdr denoms)))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                  (list-change total (cdr denoms))))
  )
)

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
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
            (cons (cons 'lambda (cons (car (zip values)) (map let-to-lambda body))) (map let-to-lambda(cadr (zip values))))
           ))
        (else
         (cons (car expr) (map let-to-lambda (cdr expr)))
         )))
