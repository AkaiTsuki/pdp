#lang scheme

;; version 10-2: The system through version 6 used a "pull" model:  the
;; ball asked its box for the position of its edges on every
;; click.  But the positions change rarely-- far less often then
;; every click.  So let's try a "push" model, in which the box updates
;; the balls when its edges change.  
;;
;; To do this, we'll have to make the balls stateful, so the box will
;; know who to notify when it becomes necessary to do so. 

;; version 6: We'll make the balls draggable.  We'll add a "selected"
;; field, so the ball will stop moving when it's selected.  
;; And we'll make a selected ball display differently.

;; version 5: OK, now we'll really add the ball factory.  The ball
;; factory, like the box, will be stateful.  It will turn out that
;; only stateful objects can add new objects to the container-- see
;; note below.

;; version 4: "n" adds a new ball.  To do this we'll do two things:
;; 1.  First, we'll write an add-object method in our container.
;; 2.  Then, we'll add a ball factory to our container.  The ball
;; factory will know about the container and the box, and on an "n" it
;; will add a ball by calling the add-object method on the container.

;; But wait: the factory needs to know about the container, so it can
;; send it add-object messages.  So this means the container must have
;; identity: it must be stateful.

;; So first, let's just make the container stateful.

;; =============================================================

;; version 3: well, that didn't work.  In this version, we'll make the
;; box stateful.  To keep track of this we'll add a new interface:
;; StatefulObj<%>. 

;; We use Void in the contracts to document this.
;; Void methods work by EFFECT.  They can return anything.
;; the contract Void means that whoever called the method promises to
;; ignore the value.

;; We'll expand the container to contain both ordinary (functional)
;; objects and stateful ones.

;; Then we'll redo Box% to be a StatefulObj<%>.

;; version 2: add a bouncing ball to the container.  The ball will
;; bounce inside a box.

;; version 1: basic architecture for worlds with multiple objects in them.

;; start this with (run).

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)

;;; Data Definitions for non-objects

;; a BallData is a (list 'Ball: Number Number Number)
;; (list 'Ball: x y speed)
(define (make-balldata x y speed)
  (list 'Ball: x y speed))
(define balldata-x second)
(define balldata-y third)
(define balldata-speed fourth)

;; a CenterData is a (list Number Number)
(define (make-centerdata x y) (list x y))
(define (centerdata-x cd) (first cd))
(define (centerdata-y cd) (second cd))

;; Stateful Objects:  they don't return a new WorldObj-- they just
;; change their state
(define StatefulWorldObj<%>
  (interface ()
    on-tick  ; ->  Void
    on-mouse ; Num Num MouseEvt -> Void
    on-key   ; KeyEvt -> Void
    add-to-scene  ; Scene -> Scene
    ))

;; functional WorldObj's.
(define WorldObj<%>
  (interface ()
    on-tick       ; -> WorldObj<%>
    on-mouse      ; Num Num MouseEvt -> WorldObj<%>
    on-key        ; KeyEvt -> world-obj
    add-to-scene  ; Scene -> Scene
    ))


(define Subscriber<%>
  (interface ()
    ;; Number -> Void
    ;; EFFECT: record new right edge
    change-edge  
    ))

(define Publisher<%>
  (interface ()
    ;; Subscriber<%> -> Void
    ;; EFFECT: add a new subscriber
    subscribe  
    ))


;; A Container% contains a list of WorldObj<%>s and StatefulWorldObj<%>s.
;; All it does is to distribute each of the world-obj methods to each
;; of its objects.
(define Container%
  (class* object% (StatefulWorldObj<%>)
    (init-field objects)  ;; ListOf<WorldObj<%>>
    (init-field stateful-objects) ;; ListOf<StatefulWorldObj<%>>
     
    (super-new)
    
    ;; WorldObj<%> -> Void
    ;; EFFECT: Add the given object to the objects field
    (define/public (add-object obj)
      (set! objects
            (cons obj objects)))

    ;; StatefulWorldObj<%> -> Void
    ;; EFFECT: Add the given stateful-object to the stateful-objects
    ;; field. 
    (define/public (add-stateful-object obj)
      (set! stateful-objects (cons obj stateful-objects)))

    ;; -> Void
    ;; EFFECT: pass on-tick to each of the functional objects, and
    ;; collect the result in the objects field; then perform the
    ;; effect on each of the stateful objects.
    (define/public (on-tick)
      (set! objects
            (map 
             (lambda (obj) (send obj on-tick))
             objects))
      (for-each
       (lambda (obj) (send obj on-tick))
       stateful-objects))

    ;; Num Num MouseEvt -> Void
    ;; We pass each mouse event to every object in the container
    ;; and let that object decide whether to respond to it.
    ;; Change is similar to on-tick
    (define/public (on-mouse x y mev)
      (set! objects
            (map
             (lambda (obj) (send obj on-mouse x y mev))
             objects))
      (for-each
       (lambda (obj) (send obj on-mouse x y mev))
       stateful-objects))
       
    
    ;; KeyEvent -> Void
    (define/public (on-key kev)
      (set! objects
            (map
             (lambda (obj) (send obj on-key kev))
             objects))
      (for-each
       (lambda (obj) (send obj on-key kev))
       stateful-objects))
    
    ;; do you see repeated code here?  
    ;; Is there an opportunity for an abstraction?
    
    ; Scene -> Scene
    ; implementation: first paint the stateful objects, then the
    ; functional ones. 
    (define/public (add-to-scene s)
      (local
       ((define s2 (foldr
                    (lambda (obj s1) (send obj add-to-scene s1))
                    s
                    stateful-objects)))
      (foldr
       (lambda (obj s1) (send obj add-to-scene s1))
       s2
       objects)))
    
    (define/public (probe)
      (list objects stateful-objects))

    ))   


;; A Box% is a rectangle
;; It will resize itself in response to mouse drags.
;; A Box is a (new Box% [x Number][y Number][w Number][h Number])
(define Box%
  (class* object% (StatefulWorldObj<%> Publisher<%>)
    (init-field
     x y ; Numbers: position of center
     w h ; Numbers: width and height, in pixels
     )

    ;; implement Publisher<%> interface
    (field [subscribers empty])  ; ListOf<Subscriber<%>>

    ;; Subscriber<%> -> Void
    ;; EFFECT: Add the subscriber to the subscriber list
    (define/public (subscribe subscriber)
      (set! subscribers (cons subscriber subscribers)))
    
    ;; Number -> Void
    ;; EFFECT: publish the value to all the subscribers
    (define (publish val)
      (for-each
        (lambda (obj)
          (send obj change-edge val))
        subscribers))

    ;; doesn't respond to ticks or key events
    (define/public (on-tick) this)
    (define/public (on-key kev) this)

    ;; on-mouse: Num Num MouseEvent -> Void
    ;; if the mouse is dragging "near" the right edge of this frame,
    ;; resize the frame to match the given mouse posn, otherwise return this
    ;; frame unchanged.  
    ;; if we move the right edge, we have to adjust the center to match
    (define/public (on-mouse mouse-x mouse-y evt)
      (if 
       (and (mouse=? evt "drag")
            (near-right-edge? mouse-x mouse-y))
       (local
        ((define new-width (- mouse-x (left-edge))))
        (adjust-width new-width))
       this))

    ;; these will be handy to have
    (define/public (left-edge) (- x (/ w 2)))
    (define/public (right-edge) (+ x (/ w 2)))
    ;; added for v5:
    ;; -> CenterData
    (define/public (center) (make-centerdata x y))

    ;; adjust-width : Number -> Void
    ;; EFFECT: adjust the center and width so that the width is
    ;; new-width and the left edge is unchanged
    ;; V7: NEW EFFECT: publish the new right edge to the subscribers
    ;; to do this, just have to restore the invariant that 
    ;; right-edge = x + w/2
    ;; left-edge = x - w/2
    ;; left-edge isn't supposed to change, so we adjust x accordingly
    ;; the objects stay in the same place on the screen, so they stay
    ;; the same.
    (define (adjust-width new-width)
      (set! x (+ (send this left-edge) (/ new-width 2)))
      (set! w new-width)
      (publish (+ x (/ w 2))))

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

    (super-new)
    
    ))


;; version 7: make Balls stateful.
;; an object of class Ball% represents a ball at the given
;; coordinates, at the given speed, living in the given box
;; INVARIANT: left-edge and right-edge are the box's left and right
;; edges. 
(define Ball%
  (class* object% (StatefulWorldObj<%> Subscriber<%>)
    (init-field
     x y        ; the position of the center of the ball
     box        ; the Box% in which the ball lives
     speed      ; ball speed in pixels/tick (either positive or negative)
     )
    
    ;; initialize right edge from the box, after that it will get
    ;; updated by messages.
    (field [right-edge (send box right-edge)])
    ;; the left edge doesn't change
    (field [left-edge (send box left-edge)])

    (field [selected? false])   ; is the ball selected?

    (define radius 15)
    
    (super-new)
    ;; at initialization, subscribe to the box's postings
    (send box subscribe this)

    ;; implement Subscriber<%> interface.
    ;; Number -> Void
    ;; EFFECT: use the received message to update right-edge
    (define/public (change-edge n)
      (set! right-edge n))

    
    (define/public (probe) (list 'Ball: x y speed))
    
    (define/public (add-to-scene s)
      (place-image
       (circle radius 
               (if selected? "solid" "outline")
               "red")
       x y s))
    
    ;; on-mouse : Num Num MouseEvent -> Void
    ;; on mouse, respond to drag, like frame:
    ;; if the mouse is dragging inside the ball, 
    ;; recenter to mouse point
    (define/public (on-mouse mouse-x mouse-y evt)
      (cond
       [(and (or (mouse=? evt "drag") (mouse=? evt "button-down"))
             (inside-this? mouse-x mouse-y))
        ;; (new Ball%
        ;;      [x mouse-x]
        ;;      [y mouse-y]
        ;;      [speed speed]
        ;;      [box box]
        ;;      [selected? true])
        (set! x mouse-x)
        (set! y mouse-y)
        (set! selected? true)]
       [(and
         (mouse=? evt "button-up")
         (inside-this? mouse-x mouse-y))
        ;; (new Ball%
        ;;      [x mouse-x][y mouse-y][box box][speed speed]
        ;;      [selected? false])
        (set! selected? false)]
       [else this]))


    ;; ball doesn't respond to  key events
    ; (define/public (on-mouse x y evt) this)
    (define/public (on-key kev) this)

    (define/public (on-tick)
      (cond
        (selected? this)
        ((would-hit-right-edge?) (place-at-right-edge))
        ((would-hit-left-edge?) (place-at-left-edge))
        (else 
          ;; (new Ball% 
          ;;   [x (+ x speed)][y y][box box][speed speed])
          (set! x (+ x speed)))))
    
    (define (place-at-right-edge)
        ;; (local 
        ;; V7: use local copy of right-edge
        ;; ((define right-edge (send box right-edge)))
        ;; (new Ball%
        ;;        [x (- right-edge radius)]
        ;;        [y y]
        ;;        [box box]
        ;;        [speed (- speed)])
        (set! x (- right-edge radius))
        (set! speed (- speed)))

    (define (place-at-left-edge)
      ; (local
        ;; v7: use local copy of left edge
        ;; ((define left-edge (send box left-edge)))
       ;; (new Ball% 
        ;;     [x (+ left-edge radius)]
        ;;     [y y]
        ;;     [box box]
        ;;     [speed (- speed)])
        (set! x (+ left-edge radius))
        (set! speed (- speed)))

    ;; wishlist functions for ball

    ;; would the right edge of the ball, travelling right, hit the
    ;; right edge?
    (define (would-hit-right-edge?)
      (>= (+ (+ x radius) speed)
        ;; (send box right-edge)
        right-edge
        ))
    
    ;; would the left edge of the ball, travelling left, hit the left
    ;; edge? 
    (define (would-hit-left-edge?)
      (<= (+ (- x radius) speed) 
        ;; (send box left-edge)
        left-edge))
    
    (define (inside-this? mouse-x mouse-y)
      (<= 
       (+
        (sqr (- x mouse-x))
        (sqr (- y mouse-y)))
       (sqr radius)))
    
    
    ))

;; BallFactory% 
;; implements StatefulWorldObj<%>
;; on key event "n", adds a new Ball at the center of the given box in
;; the given container.
;; PRECONDITION: the box must be a stateful-object in the given
;; container.
(define BallFactory%
  (class*
   object% (StatefulWorldObj<%>)

   (init-field container)        ; the Container%
   (init-field box)              ; the Box%
   (init-field [speed 3])        ; the speed of the balls to be
                                 ; created.
                                        ; we make this different from
                                        ; the initial ball.
   (super-new)

   ;; ignore on-tick, on-mouse
   (define/public (on-tick) this)
   (define/public (on-mouse x y mev) this)

   ;; don't add yourself to the scene
   (define/public (add-to-scene s) s)

   ;; KeyEvent -> Void
   ;; catch "n" events here and create a new ball. 
    (define/public (on-key kev)
      (cond
        [(key=? kev "n") 
         (send this add-ball)]
        [else this]))                   ; the else is mandatory, but
                                        ; the value is ignored.

    ;; -> Void
    ;; EFFECT: create a new ball at the center of the box, and add it
    ;; to the container as a functional^H^H^H stateful object.
    (define/public (add-ball)
      (local
       ((define the-center (send box center))
        (define the-ball
          (new Ball% 
               [x (centerdata-x the-center)]
               [y (centerdata-y the-center)]
               [box box]
               [speed speed])))
       (send container add-stateful-object the-ball)))

))

;; -> Container%
;; the initial world consists of a box and a ball, both in a
;; container.
;; the ball is functional, so it goes in "objects".
;; the box is stateful, so it goes in "stateful-objects".
(define (init-world)
  (local
   ((define the-box  (new Box% [x 100][y 45][w 150][h 75]))
    (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5]))
    (define the-container
      (new Container%
           [objects          empty]
           [stateful-objects (list the-ball the-box)])))
   (send the-container
         add-stateful-object
         (new BallFactory%
              [container the-container]
              [box       the-box]))
   the-container))

;; main function (run-world).  Create a new world and run it.
;; since on-tick, etc, return Void, we need to explicitly pass back w
;; to big-bang.
;; Container% -> Container%
(define (run-world initial-world)
  (big-bang 
   initial-world
   (on-tick
    (lambda (w) (send w on-tick) w)
    0.10)
   (on-mouse
    (lambda (w x y evt) (send w on-mouse x y evt) w))
   (on-draw
    (lambda (w) (send w add-to-scene (empty-scene 400 300))))
   (on-key
    (lambda (w kev) (send w on-key kev) w))))

;; -> Container%
(define (run) (run-world (init-world)))

;;;;;;;;;;;;;;;; wishlist functions ;;;;;;;;;;;;;;;;

;;; placed here so I could test them independently (which I didn't)

(define NEAR-VERTICAL-LINE-THRESHOLD 10)

;; Number^5 -> Boolean
;; returns true iff other-pos is "near" the line from posn1 to posn2
(define (near-vertical-line? line-x y1 y2 other-x other-y)
  (and
   (<= y1 other-y)
   (<= other-y y2)
   (<= (abs (- line-x other-x)) NEAR-VERTICAL-LINE-THRESHOLD)))

(test-begin
 (define w (init-world))
 (check-equal?
  (map length (send w probe))
  ;; changed because now everything is stateful
  '(0 3)))

(define (check-lengths w lst)
  (check-equal?
   (map length (send w probe))
   lst))

;; "test suite 1"
;; now the ball is stateful, so we'll have to add it to the stateful-objects
(test-begin
 (define the-box  (new Box% [x 100][y 45][w 150][h 75]))
 (define the-ball (new Ball% [x 100][y 45][box the-box][speed 5]))
 (define the-container
   (new Container%
        [objects          empty]
        [stateful-objects (list the-ball the-box)]))
 (check-equal?
  (map length (send the-container probe))
  '(0 2))
 (send the-container add-stateful-object (new Ball% [x 100][y 45][box the-box][speed 5]))
 (check-equal?
  (map length (send the-container probe))
  '(0 3))
 (define factory1
       (new BallFactory%
            [container the-container]
            [box       the-box]))
 (send factory1 add-ball)
 (check-lengths the-container '(0 4))
 (send the-container add-stateful-object factory1)
 (check-lengths the-container '(0 5))
 (send factory1 on-key "n")
 (check-lengths the-container '(0 6))
 (send the-container on-key "n")
 (check-lengths the-container '(0 7)))
 
;; "test suite 2"
(test-begin
 (define w (init-world))
 (send w on-key "n")
 (check-equal?
  (map length (send w probe))
  '(0 4)))


