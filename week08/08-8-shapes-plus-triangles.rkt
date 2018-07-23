#lang scheme

(require test-engine/scheme-tests)

(require 2htdp/universe)
(require 2htdp/image)

;; all geometric shapes support these methods in all contexts 
(define shape<%> 
  (interface ()
    weight ;; -> Number
    ;; compute the weight of this shape

    render ;; Scene -> Scene
    ;; add this shape to the given scene 
    ))

;; a circle 
(define circle%
  (class* object% (shape<%>)
    (init-field x  ; Number, x pixels of center from left
                y  ; Number, y pixels of center from top
		r  ; Number, radius
		c) ; ColorString 

    (define/public (weight) (* pi r r))

    (define/public (render s) (place-image IMG x y s))
    (field [IMG (circle r "solid" c)])

    (super-new)))

;; a square parallel to sides of canvas 
(define square%
  (class* object% (shape<%>)
    (init-field x  ; Number, x pixels of upper-left from left
                y  ; Number, y pixels of upper-left from top
		l  ; Number, length of one side 
		c) ; ColorString 

    (define/public (weight) (* l l))

    (define/public (render s) (place-image IMG x y s))
    (field [IMG (rectangle l l "solid" c)])
    
    (super-new)))

(define s1 (new circle% [x 50][y 30][r 40][c "red"]))
(define s2 (new square% [x 80][y 70][l 40][c "blue"]))
(define mt (empty-scene 100 100))

(check-within (send s1 weight) (* pi 1600) .1)
(check-expect (send s2 weight) 1600)
;(check-expect (image? (send s1 render mt)) true)
;(check-expect (image? (send s2 render mt)) true)
;(test)

(define (test-overlap) (send s3 render (send s2 render (send s1 render mt))))

;; =============================================================================
;; NO NEED TO REVISE THE ABOVE

;; orthogonal triangle, with two sides parallel to vertical/horizontal of canvas
(define triangle%
  (class* object% (shape<%>)
    (init-field x  ; Number, x pixels of right-angle point from left
                y  ; Number, y pixels of right-angle point from top
		v  ; Number, length of vertical side (positive means downward)
                h  ; Number, length of horizontal side (positive means rightward)
                c  ; ColorString
                ) 
    
    (define/public (render s)
      (local ((define horizontal (scene+line s x y (+ x h) y c))
              (define vertical (scene+line horizontal x y x (+ y v) c))
              (define diagonal (scene+line vertical (+ x h) y x (+ y v) c)))
        diagonal))
    
    (define/public (weight)
      (/ (* (abs v) (abs h)) 2))
    
    (super-new)))

(define s3 (new triangle% [x 30][y 90][v -10][h -20][c "green"]))

(check-expect (send s3 weight) 100)
;(check-expect (image? (send s3 render mt)) true)
(test)