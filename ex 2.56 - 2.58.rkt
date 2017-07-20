; Exercise 2.56

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

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

(define (sum? x)
  (and (pair? x) (eq? (car x) `+)))
(define (addend s) (cadr s)) ; second item of sum list
(define (augend s) (caddr s)) ; third item of sum list
(define (product? x)
  (and (pair? x) (eq? (car x) `*)))

(define (multiplier p) (cadr p)) ; second item of product list
(define (multiplicand p) (caddr p)) ; third item of product list

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (deriv (base exp) var)
          (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv `(+ x 3) `x) ; d(x + 3) / d(x)
(deriv `(* x y) `x) ; d(xy) / d(x)
(deriv `(* (* x y) (+ x 3)) `x) ; (d(xy * (x + 3)) / d(x)

; define make-exponentiation (** to denote exponentiation)
(define exp-case `(** x 4))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) `**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (make-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        (else (list `** b e))))

; Exercise 2.57
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) `+)))
(define (addend s) (cadr s)) ; second item of sum list
(define (augend s)
  (if (null? (cdddr s))
      (cadr s)
      (cons `+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) `*)))
(define (multiplier p) (cadr p)) ; second item of product list
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons `* (cddr p))))

(deriv `(* x y (+ x 3)) `x)

; Exercise 2.58 - modify differentiation program to work with infix mathematical form

(define test-case `(x + (3 * (x + (y + 2)))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 `+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 `* m2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) `+)))
(define (addend s) (car s)) ; second item of sum list
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) `*)))
(define (multiplier p) (car p)) ; second item of product list
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (deriv (base exp) var)
          (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv test-case `x)
(deriv `(5 * (x + x)) `x)

; b) Allowing standard algebraic notation
; Drop unnecessary parenthesis
; Assume multiplication is done before addition
; Patterns
; a + b * c
; a * b + c

; Idea: Insert appropriate parenthesis so that previous solution in (a) can be reused
(define test-case `(x * x + (x + 3)))
(cadr test-case); *
(append (list (list `x `* `x))  (cdddr test-case)); ((x * x) + (x + 3))
(deriv (append (list (list `x `* `x))  (cdddr test-case)) `x); 2x + 1

;(display "(1) ")(deriv `(x + 3 * (x + y + 2)) `x) ; 4
;(display "(2) ")(deriv `(x * x + 3) `x) ; 2x
;(display "(3) ")(deriv `(x + x * 3) `x) ; 1 + 3 = 4
;(display "(4) ")(deriv `(x + x + 3) `x) ; 2
;(display "(5) ")(deriv `(x * x * 4) `x) ; 8x