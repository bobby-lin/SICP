;SICP (pg 70)
;Exercise 1.36

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 20)
;#Steps = 37
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 20)
;#Steps = 15

;The averaging damping results took fewer steps than original approximation.
