#lang scheme

;; 8-5-shapes-separate-functions.rkt

;; this version uses separate functions for dispatch and operation

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

(define-struct my-circle (x y r color) #:transparent)
(define-struct my-square (x y l color) #:transparent)
(define-struct my-composite (front back) #:transparent)

;; A Shape is one of
;; -- (make-my-circle Number Number Number ColorString)
;; -- (make-my-square Number Number Number ColorString)
;; -- (make-my-composite Shape Shape)

;; interp:
;; (x,y) is position in pixels (of center for circle, of UL corner for square)
;; r is radius of circle in pixels
;; l is length of side of square, in pixels
;; c is color, expressed as a ColorString.
;; in my-composite, front is the shape in front, back is the shape in back.

;; weight : Shape -> Number
;; we assume that each shape weighs 1 gram per pixel of area(?).
;; we'll see later why we call it weight instead of area.
(define (weight s)
  (cond
    [(my-circle? s)    (my-circle-weight s)]
    [(my-square? s)    (my-square-weight s)]
    [(my-composite? s) (my-composite-weight s)]))

(define (my-circle-weight s) (* pi (my-circle-r s) (my-circle-r s)))
(define (my-square-weight s) (* (my-square-l s) (my-square-l s)))
(define (my-composite-weight s) (+ (weight (my-composite-front s))
                                   (weight (my-composite-back s))))

;; add-to-scene : Shape Scene -> Scene
;; adds the given shape to the given scene
(define (add-to-scene s scene)
  (cond
    [(my-circle? s) (my-circle-add-to-scene s scene)]
    [(my-square? s) (my-square-add-to-scene s scene)]
    [(my-composite? s) (my-composite-add-to-scene s scene)]))

(define (my-circle-add-to-scene s scene) 
  (local
    ((define IMG (circle (my-circle-r s) "solid" (my-circle-color s))))
    (place-image IMG
                 (my-circle-x s) 
                 (my-circle-y s) 
                 scene)))

(define (my-square-add-to-scene s scene)
  (local
    ((define IMG (square (my-square-l s) "solid" (my-square-color s))))
    (place-image IMG
                 (my-square-x s)
                 (my-square-y s)
                 scene)))

(define (my-composite-add-to-scene s scene)
  ;; paint the back image first, then the front image
  (add-to-scene (my-composite-front s)
    (add-to-scene (my-composite-back s)
      scene)))

(define EMPTY-CANVAS (empty-scene 200 100))

(define s1 (make-my-circle 50 20 40 "red"))
(define s2 (make-my-square 80 70 40 "blue"))
(define s3 (make-my-composite s1 s2))

(define-test-suite function-tests
  (check-= (weight s1) (* pi 1600) .1)
  (check-equal? (weight s2) 1600)
  (check-= (weight s3) (+ (* pi 1600) 1600) .1))

(run-tests function-tests)


