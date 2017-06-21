; Exercise 1.37 (a)

(define (cont-frac-recursive n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))
    )
  )
  (frac 1)
)

(cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 10)

; >> 0.6176470588235294 (k = 8)
; >> 0.6181818181818182 (k = 9)
; >> 0.6179775280898876 (k = 10)
;k must be at least 10 in order to get the approximation accuracy to 4 decimal places.

; Exercise 1.37 (b) - Iterative process
; Strategy is to track from reverse

(define (cont-frac-iter n d k)
  (define (frac i res)
    (cond
      ((= i k) (frac (- i 1) (+ (d (- i 1)) (/ (n i) (d i)))))
      ((> i 0) (frac (- i 1) (/ (n i) (+ (d i) res))))
      (else res)
    )
  )
  (frac k 0)
)

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
; >> 0.6184210526315789 (k = 9)
; >> 0.6178861788617886 (k = 10)
; >> 0.6180904522613065 (k = 11)
;k must be at least 11 in order to get the approximation accuracy to 4 decimal places.