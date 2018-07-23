#lang scheme

;; version 1: basic architecture for worlds with multiple objects in them.

;; start this with (run).
;; (run2) starts it with two boxes.

;; Exercise: make the boxes draggable, like you did in Week 1.

;; Exercise: put some flashing stars in the container.  These should
;; change from solid to outline every K ticks for some small number K.

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)

;; the methods an object in the world must respond to
(define WorldObj<%>
  (interface ()
    on-tick       ; -> WorldObj<%>
    on-mouse      ; Num Num MouseEvt -> WorldObj<%>
    on-key        ; KeyEvt -> world-obj
    add-to-scene  ; Scene -> Scene
    ))

;; A Container% contains a list of WorldObj<%>s.
;; All it does is to distribute each of the world-obj methods to each
;; of its objects.
;; A Container is a (new Container% [objects ListOf<WorldObj>]) 
(define Container%
  (class* object% (WorldObj<%>)

    (init-field objects)  ;; ListOf<WorldObj<%>>
     
    (super-new)
    
    ;; -> Container%
    (define/public (on-tick)
      (new Container%
           [objects
            (map
             (lambda (obj) (send obj on-tick))
             objects)]))

    ;; Num Num MouseEvt -> Container%
    ;; We pass each mouse event to every object in the container
    ;; and let that object decide whether to respond to it.
    ;; hmm, what if we wanted a mouse event to create or remove
    ;; objects in the world?  We'll think about that later...
    (define/public (on-mouse x y mev)
      (new Container%
           [objects
            (map
             (lambda (obj) (send obj on-mouse x y mev))
             objects)]))
    
    ;; KeyEvent -> Container%
    (define/public (on-key kev)
      (new Container%
           [objects
            (map
             (lambda (obj) (send obj on-key kev))
             objects)]))
    
    ;; do you see repeated code here?  
    ;; Is there an opportunity for an abstraction?
    
    ; Scene -> Scene
    (define/public (add-to-scene s)
      (foldr
       (lambda (obj s1) (send obj add-to-scene s1))
       s
       objects))

    ))   


;; A Box% represents a  resizeable rectangle
;; It will resize itself in response to mouse drags.
;; A Box is a (new Box% [x Number][y Number]
;;                      [w Number][h Number][selected? Boolean])
(define Box%
  (class* object% (WorldObj<%>)
    (init-field x)
    (init-field y)
    (init-field w)
    (init-field h)
     ;; x y : Numbers: position of center
     ;; w h : Numbers: width and height, in pixels
    (init-field selected?)
    
    (super-new)
    
    ;; these will be handy to have
    (define/public (left-edge) (- x (/ w 2)))
    (define/public (right-edge) (+ x (/ w 2)))

    ;; doesn't respond to ticks or key events
    (define/public (on-tick) this)
    (define/public (on-key kev) this)

    ;; on-mouse: Num Num MouseEvent -> Box%

    ;; mouse-down near right edge of box => mark box selected
    ;; drag & selected => make right edge follow mouse
    ;; mouse-up => mark box unselected

    ;; if the mouse is dragging "near" the right edge of this frame,
    ;; resize the frame to match the given mouse posn, otherwise return this
    ;; frame unchanged.  
    ;; if we move the right edge, we have to adjust the center to
    ;; match
    ;; STRATEGY: SD on evt : MouseEvent
    (define/public (on-mouse mouse-x mouse-y evt)
      (cond
        [(mouse=? evt "button-down")
         (if (near-right-edge? mouse-x mouse-y)
           (send this select)
           this)]
        [(mouse=? evt "drag")
         (if selected?
           (local
             ((define new-width (- mouse-x (left-edge))))
             (adjust-width new-width))
           this)]
        [(mouse=? evt "button-up")
         (send this unselect)]
        [else this]))

    (define/public (select)
      (new Box%
        [x x][y y][w w][h h]
        [selected? true]))

    (define/public (unselect)
      (new Box%
        [x x][y y][w w][h h]
        [selected? false])) 

    ;; adjust-width : Number -> Box%
    ;; create a new Box%, like the current one except that the
    ;; center is adjusted so that the width is new-width
    ;; to do this, just have to restore the invariant that 
    ;; right-edge = x + w/2
    ;; left-edge = x - w/2
    ;; left-edge isn't supposed to change, so we adjust x accordingly
    ;; the objects stay in the same place on the screen, so they stay
    ;; the same.
    (define (adjust-width new-width)
      (new Box%
           [x (+ (send this left-edge) (/ new-width 2))]
           [y y]
           [w new-width]
           [h h]
           [selected? selected?]))
      
    ;; you can put ordinary functions inside the class if you want
    ;; them to refer to fields
    (define (near-right-edge? other-x other-y)
      (near-vertical-line? 
       (send this right-edge)          ; the x-coordinate of the
                                       ; vertical line
       (- y (/ h 2))                   ; the y-coords of the endpoints
       (+ y (/ h 2))    
       other-x other-y))


    (define/public (add-to-scene s)
      (place-image 
       (rectangle w h "outline" "black")
       x y s))

    ))

(define-test-suite box-tests
  (check-equal?
    (local
      ((define box1 (new Box% [x 200][y 50][w 100][h 20][selected? true]))
       (define box2 (send box1 on-mouse 252 50 "drag")))
      (list
        (send box1 left-edge)
        (send box1 right-edge)
        (send box2 left-edge)
        (send box2 right-edge)))
    (list 
      150
      250
      150
      252)))



;; -> Container%
;; the initial world consists of just a container and a box.
(define (init-world)
  (new Container%
       [objects
        (list
         (new Box% [x 100][y 45][w 100][h 75][selected? false]))]))

;; main function (run-world).  Create a new world and run it.
;; Container% -> Container%
(define (run-world initial-world)
  (big-bang 
   initial-world
   (on-tick
    (lambda (w) (send w on-tick)))
   (on-mouse
    (lambda (w x y evt) (send w on-mouse x y evt)))
   (on-draw
    (lambda (w) (send w add-to-scene (empty-scene 400 300))))
   (on-key
    (lambda (w kev) (send w on-key kev)))))

;; -> Container%
(define (run) (run-world (init-world)))

;; -> Container%
;; creates a container with two boxes and runs it
(define (run2)
  (run-world
   (new Container%
        [objects
         (list
          (new Box% [x 100][y 45][w 100][h 75][selected? false])
          (new Box% [x 100][y 200][w 100][h 75][selected? false]))])))


;;;;;;;;;;;;;;;; wishlist functions ;;;;;;;;;;;;;;;;

;;; placed here so I could test them independently (which I didn't)

(define NEAR-VERTICAL-LINE-THRESHOLD 5)

;; Number^5 -> Boolean
;; returns true iff other-pos is "near" the line from posn1 to posn2
(define (near-vertical-line? line-x y1 y2 other-x other-y)
  (and
   (<= y1 other-y)
   (<= other-y y2)
   (<= (abs (- line-x other-x)) NEAR-VERTICAL-LINE-THRESHOLD)))

; can't run these tests until after near-verfical-line? is defined.
(run-tests box-tests)


