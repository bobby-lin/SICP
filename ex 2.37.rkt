; Exercise 2.37 - Basic matrix and vector operations

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      `()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))

(matrix-*-vector matrix (list 1 2 3 4))

(define (transpose mat)
  (accumulate-n cons `() mat))

(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))

(matrix-*-matrix matrix matrix)
