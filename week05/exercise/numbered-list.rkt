;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname numbered-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (numbered-list lon n)
  (cond
    [(empty? lon) empty]
    [else (cons
           (list n (first lon))
           (numbered-list (rest lon) (add1 n)))]))