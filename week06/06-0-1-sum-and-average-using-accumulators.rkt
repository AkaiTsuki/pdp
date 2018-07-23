;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 06-0-1-sum-and-average-using-accumulators) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; more simple examples with accumulators

;; Lon Number -> Number
;; GIVEN a lon which is a sublon of some larger lon
;; WHERE acc is the sum of all of the numbers in the larger lon
;;       that are to the left of the sublon
;; PRODUCES the sum of the numbers in the whole lon

(define (sum-acc sublon acc)
  (cond
    [(empty? sublon) acc]
    [else (sum-acc (rest sublon)
                   (+ (first sublon) acc))]))

(define (suml lon)
  (sum-acc lon 0))

;; Lon Number Number -> Number
;; GIVEN a sublon of some non-empty lon
;; WHERE sum-so-far is the sum of all the numbers in lon to the left of sublon
;;       AND length-so-far is the number of elements in the whole lon to the left of 
;;           sublon
;; PRODUCES the average of the elements in the lon.
(define (average-acc sublon sum-so-far length-so-far)
  (cond
    [(empty? sublon) (/ sum-so-far length-so-far)]
    [else (average-acc (rest sublon)
                       (+ (first sublon) sum-so-far)
                       (+ 1 length-so-far))]))

;; average : Lon -> Number
;; GIVEN a lon
;; WHERE lon is non-empty
(define (average lon)
  (average-acc (rest lon) (first lon) 1))
  