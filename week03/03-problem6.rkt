;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#| problem 6

   [Listof String] -> String 
   concat all strings on [Listof String] with ", " in between
|#

(require rackunit)
(require rackunit/text-ui)

(define (enumerate los)
  "")

(check-equal?
  (enumerate (list "one" "two" "three"))
  "one, two, three")







































(check-expect (enumerate.v2 '("hello" "world" "good")) "hello, world, good")

(define (enumerate.v1 los)
  (local (;; String -> String 
          ;; add a , to the front of each string 
          (define (comma+ s) (string-append ", " s)))
    ;; --- in ---
    (substring (foldr string-append "" (map comma+ los)) 2)))

(define (enumerate.v2 los)
  (local (;; String -> String 
          ;; add a , to the front of each string 
          (define (comma+ f rst) (string-append f ", " rst))
          ;; [Listof String]          
          (define rev (reverse los)))
    ;; --- in ---
    (foldl comma+ (first rev) (rest rev))))