; Arithmetic operations for Rational Numbers

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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((= n 0) (cons 0 0))
          ((< d 0) (cons (* -1 n) (* -1 d)))
          (else (cons (/ n g) (/ d g)))
    )
  )
)

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (if (or (= (numer x) 0) (= (denom x) 1))
      (display (numer x))
      ((lambda (x) (display (numer x))
       (display "/")
       (display (denom x))) x)
  )
)

;Test cases:

;Should print 0
(define neg-half (make-rat 0 -2))
(print-rat neg-half)

;Should print -3/8
(print-rat (add-rat (make-rat 1 -8) (make-rat -1 4)))

;Should print 1/8
(print-rat (sub-rat (make-rat -1 8) (make-rat 1 -4)))

;Should print 1/16
(print-rat (mul-rat (make-rat 2 8) (make-rat 1 4)))

;Should print 2
(print-rat (div-rat (make-rat 4 8) (make-rat 1 4)))

;Should print #f
(newline)
(equal-rat? (make-rat -1 8) (make-rat 1 -4))