;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |exercise 60|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct movie (title producer year))

;; TEMPLATE
;; (define (movie-fn movie)
;;   (...
;;    (movie-title movie)
;;    (movie-producer movie)
;;    (movie-year movie)))

(define-struct boyfriend (name hair eyes phone))

; TEMPLATE
;(define (boyfriend-fn b)
;  (...
;   (boyfriend-name b)
;   (boyfriend-hair b)
;   (boyfriend-eyes b)
;   (boyfriend-phone b)))

(define-struct cheerleader (name number))

;TEMPLATE
;(define (cheerleader-fn c)
;  (...
;   (cheerleader-name c)
;   (cheerleader-number c)))

(define-struct CD (artist title price))

;TEMPLATE
;(define (cd-fn cd)
;  (...
;   (cd-artist cd)
;   (cd-title cd)
;   (cd-price cd)))

(define-struct sweater (material size producer))

;TEMPLATE
;(define (sweater-fn s)
;  (...
;   (sweater-material s)
;   (sweater-size s)
;   (sweater-producer s)))