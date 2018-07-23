;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 1

   Nat -> [Listof Posn]
   create the list of diagonal points (list (make-posn 0 0) .. (make-posn n n))
|#

(require rackunit)
(require rackunit/text-ui)

(define (diagonal n)
  '())





(check-equal?
  (diagonal 3)
  (list (make-posn 0 0)
    (make-posn 1 1) 
    (make-posn 2 2) 
    (make-posn 3 3)))
