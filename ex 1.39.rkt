; Exercise 1.39 - Substitute procedures into continued fraction

(define pi 3.14)
(define (square x) (* x x))

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))

; Figure out what is the procedures for n and d.
(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (denominator i)
    (- (* i 2.0) 1))
  (cont-frac n denominator k))

; Comparison with procedure (without using other procedures as methods)
(define (tan-without-cf x k)
  (define (cf i)
    (cond
      ((= k 1) (/ x (- 1 (/ (square x) (next-term i)))))
      ((= i 1) (/ x (- 1 (/ (square x) (- (next-term i) (cf (+ i 1)))))))
      ((< i k) (/ (square x) (- ((lambda (i) (- (next-term i) 2)) i) (/ (square x) (- (next-term i) (cf (+ i 1)))))))
      ((= i k) (/ (square x) (next-term i)))))
  (cf 1))

; Test cases:
(tan (/ pi 6)) ; 0.57735026919
(display "Approximate: ")
(tan-cf (/ pi 6) 10)

(tan (/ pi 4)) ; 1
(display "Approximate: ")
(tan-cf (/ pi 4) 10)

(tan (/ pi 3)) ; 1.73205080757
(display "Approximate: ")
(tan-cf (/ pi 3) 10)