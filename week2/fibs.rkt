#lang racket

#|
The Fibonacci sequence goes like this:
F0, F1, F2, F3, F4, F5, F6, 
 0,  1,  1,  2,  3,  5,  ?, ...

|#

(define (badFib n)
  (cond
    [(zero? n) 0]
    [(equal? n 1) 1]
    [else (+ (badFib (- n 1)) (badFib (- n 2)))]))

(define (medFib n)
  (if (< n 2) n  (+ (medFib (- n 1)) (medFib (- n 2)))))

(define (goodFib n [a 0] [b 1])
  (if (< n 2) b (goodFib (- n 1) b (+ a b)))) 