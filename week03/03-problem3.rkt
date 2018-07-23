;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| PROBLEM 3

   [Listof Posn] Posn -> [Listof Posn]
   add delta-v pointwise to the given list of Posns 
|#

(require rackunit)
(require rackunit/text-ui)

(define (move-all posns delta-v) 
  '())


(check-equal?
  (move-all
    (list
      (make-posn 0 0)
      (make-posn 1 2)
      (make-posn 3 6))
    (make-posn -1 -2))
  (list
    (make-posn -1 -2)
    (make-posn 0 0)
    (make-posn 2 4)))

;; notice these values are chosen to be non-degenerate (ie, no
;; coincidences). 


