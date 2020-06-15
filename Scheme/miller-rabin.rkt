#lang racket

; Make it easier to type and read true and false booleans
(define true #t)
(define false #f)

; Abstracted squaring
(define (square x) (* x x))

; Initial call of n, the number to be tested
; 2 is always prime, and all even numbers are not prime
; If odd and not 2, go to the next step with n and n-1
(define (prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else (prime-helper n (- n 1)))))

; If a=0 then n=1 and 1 is not prime
; Perform the first miller-rabin test of n
; If returns #t, test again to reach better probability of prime
; If returns #f, not prime
(define (prime-helper n a)
  (cond ((= a 0) #f)
        ((miller-rabin-test n a) (miller-rabin-test n (- a 1)))
        (else #f)))

; If the expmod procedure returns 1 return #t
; Send it base: a (n-1), exp (n-1), m: n
; Auto set a to n-1
(define (miller-rabin-test n a)
  (= (expmod a (- n 1) n) 1))

; Exp of 0 always equals 1
; If the exp is even, check for non-trivial-sqrt recursively
; If exp is odd, get % of base * expmod base exp-1 and m
; We can divide exp by 2 if exp is even
; If odd then subtract 1 and pull it out
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (check-nontrivial-sqrt (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

; Test x=n^2%m
; If x=1 and n/=m-1 and n/=1 then return 0
; Else a non-trivial sqrt was not found so return x
(define (check-nontrivial-sqrt n m)
  (let ((x (remainder (square n) m)))
    (if (and (not (= n 1)) (not (= n (- m 1))) (= x 1))
        0
        x)))

(prime? 1)
(prime? 9)
(prime? 15)
(prime? 8911)