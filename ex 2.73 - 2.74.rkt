;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ex 2.73 - 2.74|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 2.73
; (a) Explain what was done in the procedure. Why we cannot assimilate the predicates number? and same-variable? into the data-directed dispatch?
; Now the procedure will retrieve the derivative method based on the operator given by the expression.
; The number of operators include + - * /
; We cannot categorise the types for number? and same-variable? as they cannot be listed operators in the derivative system.
; These predicates do not belong to any particular operators.

; (b) Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above?
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list `+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list `* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (install-sum-routines)
  (define (derivative ops var)
    (make-sum
     (deriv (car ops) var)
     (deriv (cadr ops) var)))
  (put 'deriv '+ derivative))

(define (install-product-routines)
  (define (derivative ops var)
    (make-sum
     (make-product (car ops)
                   (deriv (cadr ops) var))
     (make-product (deriv (car ops) var)
                   (cadr ops))))
  (put 'deriv '* derivative))

(install-sum-routines)
(install-product-routines)

(install-deriv)
(deriv `(+ x 3) `x) ; d(x + 3) / d(x)
(deriv `(* x y) `x) ; d(xy) / d(x)

; (c) Choose any additional differentiation rule that you like, such as the one for exponents (ex 2.56), and install it in this data-directed system.


; (d) In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together.
;     Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like this:
;         ((get (operator exp) 'deriv) (operands exp) var)
;     What corresponding changes to the derivative system are required?

; Previously, the operators were catgorized as types while deriv was an operation
; The procedures of the operators will now become the operations and deriv will be a type.