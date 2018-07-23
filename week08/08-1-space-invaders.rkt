#lang racket

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)

;; Press space to drop a new bomb.  
;; Bombs fall at a constant rate. 
;; Bombs are draggable.

;; Helicopter just rises at a constant rate.

;; start with (run framerate).  Typically: (run 0.25)

;; Here's what a class definition looks like:

;; classes are like data definitions.  They should have a purpose statement
;; describing what information they are supposed to represent, and
;; interpretations of the fields describing the meaning of each piece of data.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A World is a (new World% [heli Helicopter] [bombs ListOf<Bomb>])
;; interpretation: represents a world, containing a helicopter and some bombs.
(define World%               ; the % is silent
  (class* object% ()         ; the object% and () are magic incantations for now.
    ;; what you need to create an object of this class
    (init-field heli)        ; a Helicopter   --the helicopter in the game
    (init-field bombs)       ; a ListOf<Bomb> -- the list of bombs that the UFO has dropped.             

    ;; you can declare private fields, too; see below
    
    ;; this line is mandatory magic   
    (super-new)

    ;; -> World
    (define/public (after-tick)
      (new World%
        [heli    (send heli after-tick)]
        [bombs   (map
                   (lambda (bomb) (send bomb after-tick))
                   bombs)]))
    
    ;; Number Number MouseEvent -> World
    (define/public (after-mouse-event x y evt)
      (new World%
           [heli (send heli after-mouse-event x y evt)]
           [bombs (map
                   (lambda (bomb) (send bomb after-mouse-event x y evt))
                   bombs)]))
    
    ;; here the world actually does something
    ;; on space, drop a bomb, otherwise ignore
    ;; KeyEvent -> World
    ;; strategy: decomposition on KeyEvent (kev)
    (define/public (after-key-event kev)
      (cond
        [(key=? kev " ")
         (new World%
              [heli heli]
              [bombs (cons (make-bomb) bombs)])]
        [else this]))    
    
    ;; Scene -> Scene
    (define/public (to-scene scene0)
      (local
        ;; first add the helicopter to the scene
        ((define scene-with-helicopter (send heli to-scene scene0)))
        ;; then tell each bomb to add itself to the scene
        (foldr
         (lambda (bomb scene)
          (send bomb to-scene scene))
         scene-with-helicopter
         bombs)))

    ))          ; this is one of the few times we will allow you
                ; to have right parens on a line by themselves

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Bomb is a (new Bomb% [x Number][y Number]
;;                        [radius Number][selected? Boolean])
;; A Bomb represents a bomb.
(define Bomb%
  (class* object% () 
    (init-field
     x            ; the bomb's x position, in pixels, 
                  ; relative to the upper-left corner of the canvas
     y )          ; the bomb's y position
    (init-field r)   ; the bomb's radius
    (init-field [selected? false])   ; the bomb's selected? status-
                                     ; initially false.
     
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    (field [IMG (circle r "solid" "red")])     ; image for displaying the bomb
    (field [BOMBSPEED 4])                        ; the bomb's speed, in pixels/tick
       
    (super-new)
    
    ; -> Bomb
    (define/public (after-tick)
      (new Bomb% [x x][y (+ y BOMBSPEED)][r r][selected? selected?]))
    
    ; a bomb ignores key events
    ; KeyEvent -> Bomb
    (define/public (after-key-event kev)
      this)      
    
    ; Number Number MouseEvent -> World
    ; returns the bomb that should follow this one after the given
    ; mouse event.
    (define/public (after-mouse-event mouse-x mouse-y evt)
      (cond
        [(mouse=? evt "button-down")
         (send this bomb-after-button-down mouse-x mouse-y)]
        [(mouse=? evt "drag") 
         (send this bomb-after-drag mouse-x mouse-y)]
        [(mouse=? evt "button-up")
         (send this bomb-after-button-up)]
        [else this]))

    
    ;; returns the bomb after a button down.  If the event is inside
    ;; the bomb, return a bomb just like this bomb, except that it is
    ;; selected.  Otherwise return the bomb unchanged.
    (define/public (bomb-after-button-down mouse-x mouse-y)
      (if (send this in-bomb? mouse-x mouse-y)
        (new Bomb% [x x][y y][r r][selected? true])
        this))

    ;; if bomb is selected, move the bomb to the mouse location,
    ;; otherwise ignore.
    (define/public (bomb-after-drag mouse-x mouse-y)
      (if selected?
        (new Bomb% [x mouse-x][y mouse-y][r r][selected? true])
        this))

    ;; button-up unselects all bombs
    (define/public (bomb-after-button-up)
      (new Bomb% [x x][y y][r r][selected? false]))

    ; Scene -> Scene
    (define/public (to-scene scene)
      (place-image IMG x y scene))
    
    (define/public (in-bomb? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
                         
    ))

;; A Heli is a (new Heli% [x Number][y Number])
;; A Heli represents a helicopter
;; in this version, the helicopter just rises slowly.
;; in a real system, the helicopter would behave quite differently from a bomb.
(define Heli%
  (class* object% ()
    (init-field
     x            ; the heli's x position
     y            ; the heli's y position
     )
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    (field [HELI-IMG                             ; image for displaying the heli
             (square 20 "outline" "green")])
    (field [HELI-SPEED -2])                    ; the heli's speed, in pixels/tick
   
    (super-new)
    
    ; -> Heli
    (define/public (after-tick)
      (new Heli% [x x][y (+ y HELI-SPEED)]))
    
    ; Number Number MouseEvent -> World
    ; the helicopter ignores mouse events
    (define/public (after-mouse-event x y evt)
     this)
    
    ; a heli ignores key events
    ; KeyEvent -> Heli
    (define/public (after-key-event kev)
      this)    
    
    ; Scene -> Scene
    (define/public (to-scene scene)
      (place-image HELI-IMG x y scene))    

    ))

; make-bomb: -> Bomb
(define (make-bomb)
  (new Bomb% [x 75][y 0][r 10]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; setting up the world:

; -> World
(define (initial-world)
  (new World% 
       [heli (new Heli% [x 100][y 300])]
       [bombs empty]))

(define EMPTY-CANVAS (empty-scene 200 400))

; run : Number -> World
; create a world, running at rate secs/tick, and run it
(define (run rate)
  (big-bang (initial-world)
            (on-tick
              (lambda (w) (send w after-tick))
              rate)
            (on-draw
              (lambda (w) (send w to-scene EMPTY-CANVAS)))
            (on-key
              (lambda (w kev) (send w after-key-event kev)))
            (on-mouse
              (lambda (w x y evt) (send w after-mouse-event x y evt)))))


  
(define-test-suite bomb-tests  

  ;; These are exploratory hypotheses, not tests. Some of these will fail.  We
  ;; will change them to reflect what actually happens.

  ;; how is a bomb related to itself?
  
  (local ((define b1 (make-bomb)))
    (check-eqv? b1 b1
      "a bomb is eqv? to itself"))

  (local ((define b1 (make-bomb)))
    (check-equal? b1 b1
      "a bomb is equal? to itself"))
  
  ;; how are two new bombs related?

  (local ((define b1 (make-bomb))
          (define b2 (make-bomb)))
    (check-eqv? b1 b2
      "are two new bombs eqv?"))

  (local ((define b1 (make-bomb))
          (define b2 (make-bomb)))
    (check-equal?
     b1 b2
     "are two new bombs equal?"))

  ;; compare a new bomb b and (send b after-key-event " ") 
  
  (local ((define b1 (make-bomb))
          (define b2 (send b1 after-key-event " ")))
    (check-eqv?
     b1 b2
     "Is (send b1 after-key-event space) the same as b1?"))

  (local ((define b1 (make-bomb))
          (define b2 (send b1 after-key-event " ")))
    (check-equal?
     b1 b2
     "Is (send b1 after-key-event space) equal? to b1?"))

  )



#;(run-tests bomb-tests)

    
