; Exercise 2.35
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves-old x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-old (car x))
                 (count-leaves-old (cdr x))))))

(count-leaves-old (list 1 2 3 4 5))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (not (pair? x)) 1)) t)))

(count-leaves (list 1 2 3 4 5))
(count-leaves (list 1 2 3 4 5 6 7 8 9 10))