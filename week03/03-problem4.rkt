;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 4

   [Listof Posn] Number -> [Listof Posn]
   find all Posns p on posns whose x coordinate is x0 
|#

(require rackunit)
(require rackunit/text-ui)


(define (good posns x0) 
  0)



(check-equal?
 (good (list (make-posn 10 20) (make-posn 30 40) (make-posn 10 22))
       10)
 (list (make-posn 10 20) (make-posn 10 22)))

