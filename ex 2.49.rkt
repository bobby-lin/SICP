;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ex 2.49|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 2.49
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define outline-frame (list (make-segment (make-vect 0 0) (make-vect 0 0.99))
                            (make-segment (make-vect 0 0) (make-vect 0.99 0))
                            (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
                            (make-segment (make-vect 0 0.99) (make-vect 0.99 0.99))))
 
(define cross (list (make-segment (make-vect 0.0 0.0) (make-vect 0.99 0.99))
                    (make-segment (make-vect 0.0 0.99) (make-vect 0.99 0.0))))

(define diamond-shape (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                            (make-segment (make-vect 0 0.5) (make-vect 0.5 0.99))
                            (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.5))
                            (make-segment (make-vect 0.99 0.5) (make-vect 0.5 0))))

(define wave (list (make-segment (make-vect 0.5 0.3) (make-vect 0.38 0.0))
                   (make-segment (make-vect 0.5 0.3) (make-vect 0.62 0.0))
                   (make-segment (make-vect 0.82 0.0) (make-vect 0.62 0.45))
                   (make-segment (make-vect 0.62 0.45) (make-vect 0.99 0.22))
                   (make-segment (make-vect 0.99 0.40) (make-vect 0.80 0.62))
                   (make-segment (make-vect 0.80 0.62) (make-vect 0.64 0.62))
                   (make-segment (make-vect 0.64 0.62) (make-vect 0.70 0.82))
                   (make-segment (make-vect 0.70 0.82) (make-vect 0.64 0.99))
                   (make-segment (make-vect 0.44 0.62) (make-vect 0.38 0.82))
                   (make-segment (make-vect 0.38 0.82) (make-vect 0.44 0.99))
                   (make-segment (make-vect 0.44 0.62) (make-vect 0.30 0.62))
                   (make-segment (make-vect 0.30 0.62) (make-vect 0.18 0.54))
                   (make-segment (make-vect 0.18 0.54) (make-vect 0.0 0.85))
                   (make-segment (make-vect 0.18 0.30) (make-vect 0.0 0.60))
                   (make-segment (make-vect 0.18 0.30) (make-vect 0.3 0.51))
                   (make-segment (make-vect 0.3 0.51) (make-vect 0.35 0.40))
                   (make-segment (make-vect 0.35 0.40) (make-vect 0.25 0.0))))

(paint (segments->painter outline-frame))
(newline)
(paint (segments->painter cross))
(newline)
(paint (segments->painter diamond-shape))
(newline)
(paint (segments->painter wave))