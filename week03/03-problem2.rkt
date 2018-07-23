;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 2

   Nat -> [Listof Posn]
   create a list of Posns p such that p.x + p.y = n
   where x and y are non-negative integers
|#

(require rackunit)
(require rackunit/text-ui)

(check-equal? (triangle 3) (list (make-posn 0 3)
                                 (make-posn 1 2)
                                 (make-posn 2 1)
                                 (make-posn 3 0)))

