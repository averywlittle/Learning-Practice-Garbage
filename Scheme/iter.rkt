#lang racket

(define (f n)
  (define (iter a b c ctr)
    (cond ((< n 3) n)
          ((<= ctr 0) a)
          (else (iter (+ a (* 2 b)(* 3 c)) a b (- ctr 1)))))
  (iter 2 1 0 (- n 2)))

(f -1)
(f 0)
(f 5)