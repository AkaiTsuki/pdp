;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-3a-another-traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; traffic light

;; a traffic light is either
;; on regular operation
;; or flashing yellow.
;; if it is in regular operation it has:
;; a current color  
;; countdown timer (the number of ticks til the next color change)
;; if it is flashing yellow it has
;; bulb is on or off
;; countdown timeer

;; A TLColor is one of
;; -- "red"
;; -- "yellow"
;; -- "green"

(define-struct working-light (color timer))
(define-struct flashing-light (on? timer))

;; A TrafficLight is one of
;; -- (make-working-light TLColor Number)
;;     color    is the current color
;;     timer    is the number of ticks until the next color change 
;; -- (make-flashing-light Boolean Number)
;;    on? tells whether the light is currently on
;;    timer    is the number of ticks until the next color change


;; tl-fn : TrafficLight -> ??
;(define (tl-fn tl)
;  (cond
;    [(working-light? tl) (... (working-light-color tl)
;                              (working-light-timer tl))]    
;    [(flashing-light? tl) (... (flashing-light-on? tl)
;                               (flashing-light-timer tl))]))


;; 