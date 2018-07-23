;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |05-4-natural numbers|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Natural Numbers

;; A Nat is either
;; -- 0
;; -- (add1 Nat)

;; template for recursion on the natural numbers
;; fn : Nat -> ??
(define (fn n)
  (cond
    [(zero? n) ...]
    [else (... (f (sub1 n)))]))

;; define: 
;; plus (w/o accum & with)
;; times
;; exponent

