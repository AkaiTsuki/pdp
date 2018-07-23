;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 04-2-lasn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; lans and lasn

(require rackunit)
(require rackunit/text-ui)

;; A ListOfAlternatingNumbersAndStrings (LANS) is one of:
;; -- empty
;; -- (cons Number LASN)

;; A ListOfAlternatingStringsAndNumbers (LASN) is one of:
;; -- empty
;; -- (cons String LANS)

;; no interpretations because this is just a data structure exercise

(define lans0 empty)
(define lasn0 empty)
(define lans1 (cons 23 (cons "foo" (cons 11 empty))))
(define lasn1 (cons "bar" (cons 23 (cons "foo" (cons 11 empty)))))

;; lans-fn : LANS -> ??
;; (define (lans-fn lans)
;;   (cond
;;     [(empty? lans) ...]
;;     [else (...
;;             (first lans)
;;             (lasn-fn (rest lans)))]))

;; lasn-fn : LASN -> ??
;; (define (lasn-fn lasn)
;;   (cond
;;     [(empty? lasn) ...]
;;     [else (... 
;;             (first lasn)
;;             (lans-fn (rest lasn)))]))

;; lans-sum : LANS -> Number
;; Returns the sum of all the numbers in the given Lans

;; lasn-sum : LASN -> Number
;; Returns the sum of all the numbers in the given Lasn

;; strategy: structural decomposition on LANS and LASN
;; lans-sum : LANS -> Number
(define (lans-sum lans)
  (cond
    [(empty? lans) 0]
    [else (+
            (first lans)
            (lasn-sum (rest lans)))]))

;; lasn-sum : LASN -> Number
(define (lasn-sum lasn)
  (cond
    [(empty? lasn) 0]
    [else (lans-sum (rest lasn))]))

(check-equal? (lans-sum lans1) 34)
(check-equal? (lasn-sum lasn1) 34)
