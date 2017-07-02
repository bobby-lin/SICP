(define zero (lambda (f) (lambda (x) x)))

; n is the number of times to add.
; f is the function
; x is the item to add to.

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Task: Define one and two directly (no in terms of zero and add-1)

; To define one, we use f once.
(define one
  (lambda (f)
    (lambda (x) (f x))))
; To define two, we use f two times.
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define four
  (lambda (f)
    (lambda (x)
      (f (f (f (f x)))))))

(define print-str (lambda (x)
    (display x)
    (newline)
    x))

(((add-1 four) print-str) "Hi") ; 4 + 1 (Print "Hi" five times)

; Application: ((n f) x), n is the number of times to add, f is the function, and x is the item
; In this example, the procedure reads n as two times (function-to display x is implemented twice), and x is the string "hello".
((four print-str) ;x is returned for the next iteration
 "hello") 

; Give a direct definition of add (not in terms of repeated application of add-1)
(define (add a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))

; Description: We will execute f on x and the return x by b times. Then we will continue the execution of f and the return x by a times.
(((add one two) print-str) "hello")


  