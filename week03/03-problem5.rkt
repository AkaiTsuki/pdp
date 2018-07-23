;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 5
 
   [Listof Posn] Number -> [Listof Number]
   extract the y coordinates from those Posns on lop whose x coordinate is x0
|#

(define (extract posns x0)
  '())


(check-equal?
 (good (list (make-posn 10 20) (make-posn 30 40) (make-posn 10 22))
       10)
 (list 20 22))
