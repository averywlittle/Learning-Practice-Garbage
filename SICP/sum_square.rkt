#lang racket
(define (square x) (* x x))

(define (sum_square a b c)
  (cond ((and (< a b)(< a c))(+ (square b)(square c)))
        ((and (< b c)(< b a))(+ (square a)(square c)))
        (else(+ (square a)(square b)))))

(sum_square 3 2 4)