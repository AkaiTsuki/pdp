;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname time-structure) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct Time-Point(hh mm ss))

;; A Time-Point is a structure: (make-Time-Point Number Number Number)
;; hh is the hour of the time point.
;; mm is the minute of the time point.
;; ss is the second of the time point.

(make-Time-Point 10 10 10)