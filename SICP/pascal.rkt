#lang racket

;recursive
(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))

(pascal 1 1)
(pascal 5 2)

