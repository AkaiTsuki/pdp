;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |exercise 61|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct Time-Point(hh mm ss))

;; A Time-Point is a structure: (make-Time-Point Number Number Number)
;; (make-Time-Point hh mm ss) is the a time point that
;; hh is the hour of the time point;
;; mm is the minute of the time point;
;; ss is the second of the time point.

;time->seconds: Time-Point -> Number
;produces the number of seconds that have passed since midnight
;Strategy: Structural Decomposition on t : Time-Point
;example

(define t (make-Time-Point 12 30 2))
(check-expect (time->seconds t) 45002)

(define (time->seconds t)
  (+ (+ (hour->seconds (Time-Point-hh t))
        (minute->seconds (Time-Point-mm t)))
     (Time-Point-ss t)))

;hour->seconds Number->Number
;produces the number of seconds from h, a given number of hour.
;example
(check-expect (hour->seconds 10) (* 10 3600))

(define (hour->seconds h)
  (* h 3600))


;minute->seconds Number->Number
;produces the number of seconds from m, a given number of minute.
;example
(check-expect (minute->seconds 5) (* 5 60))

(define (minute->seconds m)
  (* m 60))


