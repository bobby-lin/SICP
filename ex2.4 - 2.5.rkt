; Exercise 2.4 - Implementing selectors differently
; Verify whether (car (cons x y)) yields for x for any objects x and y.
; Yes. We can subsitute the procedure from (cons x y) into (car z)
; ((lambda (m) (m x y)) (lambda (p q) p))
; m will be the method of "(lambda (p q) p)"
; Given that (lambda (p q) p) always return p, then lambda (x y) will return x.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z lambda (p q) p))

What is the corresponding definition of cdr? (Hint: Verify using substitution model as learned in section 1.1.5.)
(define (cdr z)
  (z lambda (p q) q))

; Exercise 2.5
; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations
; if we represent the pair a and b as the integer that is the product (2^a) * (3^b)
; Give the corresponding def of the procedures cons, car and cdr.
(define (cons-modified a b)
  (* (expt 2 a) (expt 3 b)))
(cons-modified 5 8)

; How to retrieve a or b for the procedures car and cdr?
(define (find-power integer base)
  (define (div-iter result count)
    (if (= (/ result base) 1)
        (+ count 1)
        (div-iter (/ result base) (+ count 1))
        ))
  (div-iter integer 0))

(define (find-paired-value divisor base integer)
  (define (num-div result)
    (if (= (remainder result divisor) 0)
        (num-div (/ result divisor))
        (find-power result base)))
  (num-div integer))

(define (car-modified z)
  (find-paired-value 3 2 z))

(define (cdr-modified z)
  (find-paired-value 2 3 z))

(find-paired-value 2 3 72) ; 2^3 * 3^2 = 8 * 9 = 72
(find-paired-value 3 2 209952) ; 2^5 * 3^8 = 209952
(define test-cons-case (cons-modified 5 8))
(car-modified test-cons-case) ; Should return 5
(cdr-modified test-cons-case) ; Should return 8
(define test-cons-case (cons-modified 2.0 12.0))
(car-modified test-cons-case) ; Should return 2.0
(cdr-modified test-cons-case) ; Should return 12.0
