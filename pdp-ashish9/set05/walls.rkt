;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname walls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide mouse-handler)
(provide end?)
(provide get-balls)
(provide mk-ball)
(provide replace-balls)
(provide get-ball-x)
(provide get-ball-y)
(provide score)
(provide level)



; GLOBAL CONSTANTS
;__________________________________________________________________________

(define TIME-ON-TASK 36) ;hours
(define HEIGHT 400)   ; height of canvas
(define WIDTH 400)   ; width of canvas
(define CENTER-Y (/ HEIGHT 2)); Center of canvas
(define CENTER-X (/ WIDTH 2)); Center of canvas
(define BALL-RADIUS 20 )  ; radius of ball
(define ZERO 0 )   ; pixels
(define BALL-IMG (circle BALL-RADIUS "solid" "blue")) ; image of ball
(define MTS (empty-scene HEIGHT WIDTH)) ; image of empty scene
(define BALL-LEFT-LIMIT BALL-RADIUS )  ; left edge of canvas
(define BALL-RIGHT-LIMIT (- WIDTH BALL-RADIUS)) ; right edge of canvas
(define BALL-TOP-LIMIT BALL-RADIUS )   ; top edge of canvas
(define BALL-BOTTOM-LIMIT (- HEIGHT BALL-RADIUS)); bottom edge of canvas 
(define TEXTSIZE 20 ) ;pixels
(define TEXTCOLOR "black") ;String
(define TEXT-X-POS 150)  ;pixels
(define TEXT-Y-POS 350)  ;pixels
(define WALL-GROW-RATE 8) ;pixels
(define END-GAME "Game Over") ; String
(define END-IMAGE (text END-GAME TEXTSIZE TEXTCOLOR))


; DATA DEFINITION
;__________________________________________________________________________

; An Axis is one of 
; - "Horizontal"
; - "Vertical"
; INTERP :  represents major axis of a wall.
(define HOR "HORIZONTAL")
(define VER "VERTICAL")

; <Axis predicates> : Axis -> Boolean
; Returns true if a is the Axis indicated by the function name.
; Strategy:function composition
(define (hor? d) (string=? d HOR))
(define (ver? d)  (string=? d VER))

; TEMPLATE
; axis-fn Axis -> ???
;(define (axis-fn a)
;  (cond
;    [(hor? a) ...]
;    [(ver? a) ...]))

;-------------------------------------------------------------------------
; A MouseEvent is one of:
; - "button-down"
; INTERP: represents a mouse click in a Universe (big-bang ) program

; TEMPLATE:
; mouse-event-fn : MouseEvent -> ???
;(define (mouse-event-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [else ...]))

;--------------------------------------------------------------------------
; A KeyEvent is one of:
; - " "
; INTERP: A KeyEvent represents a key-press in Universe (big-bang) program.

; TEMPLATE:
; keyevent-fn : KeyEvent -> ???
;(define (keyevent-fn kev)
;  (cond
;    [(key=? kev " ") ...] 
;    [ else ...]))

;--------------------------------------------------------------------------
; A Ball is a (make-ball Coordinate Coordinate Integer Integer) 
; represents a ball that moves around in the free space.
(define-struct ball( x y vx vy ))

#|TEMPLATE
ball-fn : Ball -> ???
(define (ball-fn ball)
(... (ball-x ball)
... (ball-y ball)
... (ball-vx ball)
... (ball-vy ball)
...)) |#

;Example

(define  BALL1 (make-ball 28 37 8 7  ))
(define  BALL2 (make-ball 32 57 2 5  ))
(define  BALL3 (make-ball  90 120  4 3))

;--------------------------------------------------------------------------
;A NonEmptyListofBall is a (cons Ball ListOfBall)

;A ListOfBall is one of
; - empty 
; - (cons Ball ListOfBall)
;INTERP: represents a list of balls in a world
; World will always have atleast one ball

; Example
(define LV2-BALL-LIST (cons BALL1 (cons BALL2 '())))
(define LV3-BALL-LIST (cons BALL3  LV2-BALL-LIST))
#|TEMPLATE

ListOfBall-fn : ListOfBall -> ???
(define (ListOfBall-fn lst)
 (cond
      [(empty? lst) ...]
      [else (... (ball-fn(first lst))
                 (ListOfBall-fn(rest lst)))]))
|#


;--------------------------------------------------------------------------

;A Wall is a (make-wall ( Coordinate Coordiante Coordinate Coordinate )
; represents a wall that is created on every click .
;INTERP: Wall are either parallel or perpendicular to x-axis 
;  Coordinates are the position of end points of the wall
(define-struct wall( x1 y1  x2 y2 ))

#|TEMPLATE
wall-fn : Wall -> ???
(define (wall-fn wall)
(... (wall-x1 wall)
... (wall-y1 wall)
... (wall-x2 wall)
... (wall-y2 wall)
...)) |#

;EXAMPLE
(define WALLY (make-wall  40 30 40 300  ))
(define WALLX ( make-wall  30 35  120 35 ))
(define WALLX2 ( make-wall  30 105 40 105 ))
(define EDGE-LEFT (make-wall  0 0  0 400))
(define EDGE-RIGHT (make-wall  400 0 400 400))
(define EDGE-TOP (make-wall  0 0  400 0))
(define EDGE-BOTTOM (make-wall  0 400 400 400))
(define WALL-ZERO (make-wall 0 0 0 0))
;--------------------------------------------------------------------------
;A NonEmptyListOfWall (NELoW) is a (cons EDGE-LEFT (cons EDGE-RIGHT 
;                              ( cons EDGE-TOP (cons EDGE-BOTTOM 
;                               ListOfWall)

;A ListOfWall is one of
; -  empty
; - (cons Wall ListOfWall)
;INTERP: represents a list of walls in a world
; World will always have four edges as walls

; Example
(define WALL-LIST1 (cons WALLY (cons WALLX '())))
(define WALL-INIT (cons EDGE-LEFT 
                        (cons EDGE-RIGHT 
                              ( cons EDGE-TOP 
                                     (cons EDGE-BOTTOM '())))))
(define WALL-LIST-TEST (cons (make-wall 150 0 150 400)
                             (cons (make-wall 0 120 150 120)
                                   WALL-INIT)))
#|TEMPLATE
ListOfWall-fn : ListOfWall -> ???
(define (ListOfWall-fn lst)
 (cond
      [(empty? lst) ...]
      [else (... (wall-fn (first lst))
                 (ListOfWall-fn(rest lst)))]))
|#

;--------------------------------------------------------------------------

;A World is a 
;(make-world (NELoB NELoW Natural Wall Boolean PosInt)
; represents a World of where balls in list of ball are moving with 
;random velocities 
;INTERP : Lob Low are the list of balls and walls respectively in 
;current scene . axis determine along whch axis wall will grow
(define-struct world ( LoB LoW score wall axis level grow))

#|TEMPLATE
world-fn : World -> ???
(define (world-fn world)
(...  (ListOfBall-fn(world-LoB world))
...   (ListOfWall-fn(world-LoW world))
...  (world-score world)
...   (wall-fn (world-wall world))
...   (axis-fn (world-wall world)
...   (world-level world)
...)) |#

;EXAMPLE
(define WORLD 
  (make-world LV2-BALL-LIST WALL-INIT ZERO WALLX2  HOR 3 #false))
(define INITIAL-WORLD (make-world
                       (list (make-ball 45 23 2 7 )) WALL-INIT 0 
                       (make-wall 0 0 0 0 ) VER 1 #f))
(define INITIAL-WORLD-TRUE (make-world
                            (list (make-ball 45 23 2 7 )) WALL-INIT 0 
                            (make-wall 0 0 0 0 ) VER 1 #t))

(define INITIAL-WORLD-HOR (make-world
                           (list (make-ball 45 23 2 7 )) WALL-INIT 0 
                           (make-wall 0 0 0 0 ) HOR 1 #f))

(define WORLD2 (make-world
                (list (make-ball 45 23 2 7 )
                      (make-ball 36 79 3 4)) WALL-INIT 0 
                                             (make-wall 0 0 0 0 ) VER 1 #f))
;--------------------------------------------------------------------------
#|
                FUNCTIONS
|#
;--------------------------------------------------------------------------

;run :World -> World
;Starts the Stimulation
;STRATEGY: Function Composition
(define (run w)
  (big-bang w
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)
            (on-draw render)
            ;(stop-when end? render-last)
            ))


;--------------------------------------------------------------------------
#|
           AUXILIARY FUNCTION
|#
;--------------------------------------------------------------------------
; get-ball-x : Ball -> Coordinate
; Returns the x position of the Ball's center.
; Example:
(begin-for-test
  (check-equal? (get-ball-x(make-ball 244 380 -8 -7 ))
                244
                "x position of center of ball"))
;STRATEGY : Data Decomposition of b : Ball
(define (get-ball-x b)
  (ball-x b))


;--------------------------------------------------------------------------
; get-ball-y : Ball -> Coordiate
; Returns the y position of the Ball's center.
; Example:
(begin-for-test
  (check-equal? (get-ball-y(make-ball 244 380 -8 -7  ))
                380
                "y position of center of ball"))
;STRATEGY : Data Decomposition of b : Ball
(define (get-ball-y b)
  (ball-y b))

;--------------------------------------------------------------------------
; get-balls : World -> ListOf<Ball>
; Returns all the balls in the given world.  
; Example:
(begin-for-test
  (check-equal? (get-balls WORLD)
                (list 
                 (make-ball 28 37 8 7 ) 
                 (make-ball 32 57 2 5 ))
                "list of balls of WORLD"))
;STRATEGY : Data Decomposition on w : WORLD
(define (get-balls w)
  (world-LoB w))
;--------------------------------------------------------------------------
; score : World -> Natural
; Returns the current score.
;Example
(begin-for-test
  (check-equal? (score WORLD)
                0
                "Initial Score"))
;STRATEGY: Data Decomposition on w: World
(define (score w )
  (world-score w))

;--------------------------------------------------------------------------
; level : World -> Natural
; Returns the current level.
;Example:
(begin-for-test
  (check-equal? (level WORLD)
                2
                "Level 2"))
;STRATEGY: Data decomposition on w: World
(define (level w)
  (length (world-LoB w)))

;-------------------------------------------------------------------------

; get-first-list : Listof<X> -> X
;Will return first element of lst
;Example (first (list 1 3 4 )) -> 1
;STRATEGY: Data Decomposition of lst : ListOf<X>
(define (get-first-list lst)
  (first lst))
;-------------------------------------------------------------------------
; mk-ball : Coordinate Coordinate Real Real -> Ball
; Returns a Ball with center at (x,y), with the given velocities.
; A positive x velocity is in the x-increasing direction and vice versa.
; The y velocity is similar.
;Example 
(begin-for-test 
  (check-equal? 
   (mk-ball 32 33 3 5 )
   (make-ball 32 33 3 5  )
   "new -bal is created "))
;STRATEGY : Function Composition

(define (mk-ball x y vx vy)
  (make-ball
   x
   y
   vx
   vy))
;-------------------------------------------------------------------------
; replace-balls : World ListOf<Ball> -> World
; Replaces the Balls in the given World with the given Balls.
;; Example :
(begin-for-test 
  (check-equal? 
   (replace-balls INITIAL-WORLD (list (make-ball 30 40 2 3 )
                                      (make-ball 30 50 4 5 )))
   (make-world
    (list
     (make-ball 30 40 2 3)
     (make-ball 30 50 4 5 ))
    WALL-INIT
    0
    (make-wall 0 0 0 0 )
    "VERTICAL"
    1
    false)
   "new replaced world"))
;STRATEGY : Data Decomposition on w : World
(define (replace-balls w lob)
  (make-world
   lob
   (world-LoW w)
   (world-score w)
   (world-wall w)
   (world-axis w)
   (world-level w)
   (world-grow w)))

;-----------------------------------------------------------------------
#|              
                        DRAW-WORLD
|#
;-----------------------------------------------------------------------


;render :World -> image
;renders the current world state w
;example
(begin-for-test
  (check-equal? (render WORLD2)
                (place-image 
                 (text 
                  (string-append 
                   "Click for " 
                   VER 
                   " Wall") 
                  TEXTSIZE
                  TEXTCOLOR )
                 TEXT-X-POS 
                 TEXT-Y-POS
                 (render-ball
                  (list
                   (make-wall 0 0 0 400)
                   (make-wall 400 0 400 400)
                   (make-wall 0 0 400 0)
                   (make-wall 0 400 400 400))
                  (list (make-ball 45 23 2 7)
                        (make-ball 36 79 3 4))
                  (make-wall 0 0 0 0)   ))
                "world image "))


;STRATEGY : Data Decomposition of w : World
(define (render w)
  (place-image 
   (text 
    (string-append 
     "Click for " 
     (world-axis w) 
     " Wall") 
    TEXTSIZE
    TEXTCOLOR )
   TEXT-X-POS 
   TEXT-Y-POS
   (render-ball (world-LoW w) (world-LoB w) (world-wall w))))


;render-ball :NELoB NELoW Wall -> image
;renders each ball of the list.
;STRATEGY : Data Decomposition of w ,lb : World, NELoB
(define (render-ball lw lb w)
  (cond 
    [(empty? (rest lb)) (place-image BALL-IMG 
                                     (get-ball-x (first lb))
                                     (get-ball-y (first lb))
                                     (add-line (render-wall lw)
                                               (wall-x1 w) 
                                               (wall-y1 w)
                                               (wall-x2 w) 
                                               (wall-y2 w) 
                                               (make-pen "red" 4
                                                         "solid" "round" 
                                                         "round")))]
    [else (place-image BALL-IMG 
                       (get-ball-x (first lb))
                       (get-ball-y (first lb))
                       (render-ball lw (rest lb) w))]))



;--------------------------------------------------------------------------
; render-wall :NELoW -> Image 
;renders each wall of the list

;STRATEGY : Data Decomposition of lw: NELoW
(define (render-wall lw)
  (cond
    [(empty? lw) MTS]
    [else (render-each-wall (first lw) (render-wall (rest lw)))]))
;--------------------------------------------------------------------------
; render-each-wall : Wall-> Image
; renders each wall seperately

;STRATEGY: Data Decomposition of wll : Wall

(define (render-each-wall wll img)
  (add-line img (wall-x1 wll) (wall-y1 wll) (wall-x2 wll) (wall-y2 wll) 
            (make-pen "red" 4 "solid" "round" "round")))


;;-----------------------------------------------------------------------
#|              
                        KEY HANDLER
|#
;;-----------------------------------------------------------------------
; key-handler : World KeyEvent -> World
; Computes the next world if world is not growing
;Example
(begin-for-test
  (check-equal?  (key-handler INITIAL-WORLD " ")
                 (make-world
                  (list (make-ball 45 23 2 7))
                  (list
                   (make-wall 0 0 0 400)
                   (make-wall 400 0 400 400)
                   (make-wall 0 0 400 0)
                   (make-wall 0 400 400 400))
                  0
                  (make-wall 0 0 0 0)
                  "HORIZONTAL"
                  1
                  false)
                 "vertical to horizontal"))
;STRATEGY: Data Decomposition on w : World
(define (key-handler w key)
  (if (world-grow w)
      w
      (world-after-key w key)))

;Testing:
(begin-for-test
  (check-equal?
   (key-handler INITIAL-WORLD-TRUE " ")
   INITIAL-WORLD-TRUE
   "ignores KeyEvent"))

;;----------------------------------------------------------------------
;; world-after-key : World KeyEvent -> World
; Computes the next world after a key press.
;Example :
(begin-for-test
  (check-equal? (world-after-key INITIAL-WORLD " ")
                INITIAL-WORLD-HOR
                "CHangeAxis"))

;STRATEGY : Data Decomposition on key: KeyEvent
(define (world-after-key w key )
  (cond 
    [(key=? key " ") (change-world w)] 
    [else w]))

;Testing
(begin-for-test
  (check-equal? (world-after-key INITIAL-WORLD "l")
                INITIAL-WORLD
                "Ignores KeyEvent"))

;;-----------------------------------------------------------------------
; change-world : World -> World
; will return new world with change in axis
; Example;
(begin-for-test
  (check-equal?
   (change-world INITIAL-WORLD)
   INITIAL-WORLD-HOR
   "change axis"))

; STRATEGY : Data Decomposition on w: World
(define (change-world w)
  (make-world
   (world-LoB w)
   (world-LoW w)
   (world-score w)
   (world-wall w)
   (change-world-axis (world-axis w))
   (world-level w)
   (world-grow w)))


;;-----------------------------------------------------------------------
; change-world-axis : Axis -> Axis 
; Will change the axis of the world
; Example
(begin-for-test
  (check-equal? (change-world-axis HOR)
                VER
                "changes axis"))
; STRATEGY : Data Decomposition on a : Axis
(define (change-world-axis a)
  (cond
    [(hor? a) VER]
    [(ver? a) HOR]))




;;-----------------------------------------------------------------------
#|              
                        MOUSE HANDLER
|#
;-------------------------------------------------------------------------
;mouse-handler: World Coordinate Coordinate MouseEvent -> World
;will return new world if world is not growing

;Example
(begin-for-test
  (check-equal?  (mouse-handler INITIAL-WORLD 40 50 "button-down")
                 (make-world
                  (list (make-ball 45 23 2 7))
                  (list
                   (make-wall 0 0 0 400)
                   (make-wall 400 0 400 400)
                   (make-wall 0 0 400 0)
                   (make-wall 0 400 400 400))
                  0
                  (make-wall 40 49 40 51)
                  "VERTICAL"
                  1
                  true)
                 "vertical to horizontal"))

;STRATEGY: Data Decomposition on w : World
(define (mouse-handler w x y mev)
  (if (world-grow w)
      w
      (world-after-click w x y mev)))
;Testing
(begin-for-test
  (check-equal?
   (mouse-handler INITIAL-WORLD-TRUE 40 50 "button-down")
   INITIAL-WORLD-TRUE
   "ignores click")) 
;;-----------------------------------------------------------------------
;world-after-click:  World Coordinate Coordinate MouseEvent -> World
;will return new world based on mouse click
;example

(begin-for-test
  (check-equal?
   (mouse-handler INITIAL-WORLD 40 50 "button-up")
   INITIAL-WORLD
   "ignores click"))
;STRATEGY: Data Decomposition on mev: MouseEvent
(define (world-after-click w mx my mev)
  (cond
    [(mouse=? mev "button-down")  (new-world-after-click w mx my)]
    [else w]))

;;-----------------------------------------------------------------------
;new-world-after-click: World Coordinate Coordinate -> World
;will create a new world with wall starting to grow at
;example 
(begin-for-test
  (check-equal?
   (new-world-after-click INITIAL-WORLD 12 13 )
   (make-world
    (list (make-ball 45 23 2 7))
    (list
     (make-wall 0 0 0 400)
     (make-wall 400 0 400 400)
     (make-wall 0 0 400 0)
     (make-wall 0 400 400 400))
    0
    (make-wall 12 12 12 14)
    "VERTICAL"
    1
    true)
   "change axis"))
;STRATEGY: Data Decomposition on w: World
(define (new-world-after-click w x y )
  (make-world
   (world-LoB w)
   (world-LoW w)
   (world-score w)
   (new-wall x y (world-axis w))
   (world-axis w)
   (world-level w)
   (not (world-grow w))))
;;-----------------------------------------------------------------------
;new-wall : Coordinate Coordiante Axis -> Wall
;Will make a new horizontal or vertical Wall
; Example
(begin-for-test
  (check-equal? (new-wall 35 56 HOR)
                (make-wall 34 56 36 56)
                "change-axis"))
;Strategy : Data Decomposition on a: Axis
(define (new-wall x y a)
  (cond
    [(hor? a) (make-wall (sub1 x) y (add1 x) y)]
    [(ver? a) (make-wall x (sub1 y) x (add1 y))]))


;;-----------------------------------------------------------------------
#|              
                        NEXT WORLD
|#
;-------------------------------------------------------------------------
; next-world : World -> World
; will return Next world on each tick from the currents  state
; Example :
(begin-for-test
  (check-equal?
   (next-world INITIAL-WORLD)
   (make-world
    (list (make-ball 47 30 2 7))
    (list
     (make-wall 0 0 0 400)
     (make-wall 400 0 400 400)
     (make-wall 0 0 400 0)
     (make-wall 0 400 400 400))
    0
    (make-wall 0 0 0 0)
    "VERTICAL"
    1
    false)
   "nextworld"))

;STRATEGY: Data Decomposition on w : World
(define (next-world w)
  (if (and 
       (world-grow w)
       (not (world-grow (next-world-grow-wall w))))
      (next-world-new-wall w)
      (next-world-grow-wall w)))

;Testing :
(begin-for-test
  (check-equal?
   (next-world INITIAL-WORLD-TRUE)  
   (make-world
    (list (make-ball 47 30 2 7))
    (list
     (make-wall 0 0 0 0)
     (make-wall 0 0 0 400)
     (make-wall 400 0 400 400)
     (make-wall 0 0 400 0)
     (make-wall 0 400 400 400))
    0
    (make-wall 0 0 0 0)
    "VERTICAL"
    1
    false)
   "next-grow-world"))

;;-----------------------------------------------------------------------
; next-world-new-wall : World -> World
; will return next world if new wall is completely formed
;Example
(begin-for-test
  (check-equal? (next-world-new-wall INITIAL-WORLD-TRUE) 
                (make-world
                 (list (make-ball 47 30 2 7))
                 (list
                  (make-wall 0 0 0 0)
                  (make-wall 0 0 0 400)
                  (make-wall 400 0 400 400)
                  (make-wall 0 0 400 0)
                  (make-wall 0 400 400 400))
                 0
                 (make-wall 0 0 0 0)
                 "VERTICAL"
                 1
                 false)
                "NewWllToWOrld"))
;STRATEGY: Data Decomposition on w: World
(define (next-world-new-wall w) 
  (make-world 
   (next-balls (world-LoB w) (world-LoW w))
   (add-wall w) 
   (world-score w)
   (next-wall w)
   (world-axis w)
   (level w)
   (check-grow w)))

;;-----------------------------------------------------------------------
;next-world-grow-world :World -> World
;computes the next world state from given world state
;Example
(begin-for-test
  (check-equal?
   (next-world-grow-wall INITIAL-WORLD-TRUE) 
   (make-world
    (list (make-ball 47 30 2 7))
    (list
     (make-wall 0 0 0 400)
     (make-wall 400 0 400 400)
     (make-wall 0 0 400 0)
     (make-wall 0 400 400 400))
    0
    (make-wall 0 0 0 0)
    "VERTICAL"
    1
    false)
   "grow world"))

;STRATEGY: Function Composition
(define (next-world-grow-wall w) 
  (make-world 
   (next-balls (world-LoB w) (world-LoW w))
   (world-LoW w) 
   (world-score w)
   (next-wall w)
   (world-axis w)
   (level w)
   (check-grow w)))
;;-----------------------------------------------------------------------
;add-wall : World -> NELoW
;will add wall to the list of walls
;example:
(begin-for-test
  (check-equal?
   (add-wall WORLD)
   (list
    (make-wall 30 105 40 105)
    (make-wall 0 0 0 400)
    (make-wall 400 0 400 400)
    (make-wall 0 0 400 0)
    (make-wall 0 400 400 400))
   "new wall added"))
;STRATEGY: Data Decomposition on w: World

(define (add-wall w)
  (cons (world-wall w) (world-LoW w)))

;;-----------------------------------------------------------------------
;next-wall: World -> Wall
;will return growing wall or exsiting wall of the world
;Example
(begin-for-test
  (check-equal? (next-wall WORLD)
                (make-wall 30 105 40 105)
                "new wall "))
;STRATEGY: Data Decomposition on w: World
(define (next-wall w)
  (if (world-grow w) 
      (grow-wall (world-wall w) (world-axis w) (world-LoW w) )
      (world-wall w)))
;--------------------------------------------------------------------
;grow-wall : Wall Axis NELoW -> Wall
;will return a wall that is active
;example
(begin-for-test
  (check-equal?
   (grow-wall (make-wall 10 20 30 20 ) HOR WALL-INIT)
   (make-wall 2 20 38 20)
   "new wall at horizontal direction"))
;STRATEGY: Data Decomposition on a : Axis
(define (grow-wall wll a lw)
  (cond
    [(hor? a) (grow-horizontal-wall  wll lw)]
    [(ver? a) (grow-vertical-wall wll lw)]))
;--------------------------------------------------------------------
;grow-horizontal-wall: Wall NELoW -> Wall
;will grow wall in horizontal direction
;Example
(begin-for-test
  (check-equal?
   (grow-horizontal-wall WALLX WALL-INIT)
   (make-wall 22 35 128 35)
   "wall grew"))
;STRATEGY: Data Decomposition on wll : Wall
(define (grow-horizontal-wall wll lw)
  (make-wall  
   (grow-wall-left wll lw)
   (wall-y1 wll)
   (grow-wall-right wll lw)
   (wall-y1 wll)))
;-------------------------------------------------------------------
;grow-vertical-wall: Wall NELoW -> Wall
;will grow wall in Vertical direction
;Example
(begin-for-test
  (check-equal?
   (grow-vertical-wall WALLX WALL-INIT)
   (make-wall 30 27 120 43)
   "wall grew"))
;STRATEGY: Data Decomposition on wll : Wall
(define (grow-vertical-wall wll lw)
  (make-wall 
   (wall-x1 wll) 
   (grow-wall-up wll lw)
   (wall-x2 wll)
   (grow-wall-down wll lw)))

;------------------------------------------------------------------
;grow-wall-left : Wall NELoW ->Coordinate
;will return x coordinate of left end of wall
;Example
(begin-for-test 
  (check-equal? 
   (grow-wall-left 
    (make-wall 10 30 50 30)
    WALL-INIT)
   2
   "left coordinateof wall"))
;STRATEGY data Decomposition on wll : Wall
(define (grow-wall-left wll lw)
  (local (
          ;Will return new coordinate without limit
          (define next-pos (- (wall-x1 wll) 
                              WALL-GROW-RATE))
          ;Will return boundary of wall
          (define stop-at (get-left-limit  
                           (+ (wall-x1 wll) WALL-GROW-RATE)
                           (wall-y1 wll)
                           lw)))
    (if (< stop-at (wall-x1 wll))
        next-pos
        stop-at)))
;------------------------------------------------------------------
;grow-wall-right : Wall NELoW ->Coordinate
;will return x coordinate of right end of wall
;Example
(begin-for-test 
  (check-equal? 
   (grow-wall-right 
    (make-wall 10 30 50 30)
    WALL-INIT)
   58
   "right coordinateof wall"))
;STRATEGY data Decomposition on wll : Wall

(define (grow-wall-right wll lw)  
  (local (
          ;Will return new coordinate without limit
          (define next-pos (+ (wall-x2 wll)
                              WALL-GROW-RATE))    
          ;will return boundary of wall
          (define stop-at (get-right-limit
                           (- (wall-x2 wll) WALL-GROW-RATE)
                           (wall-y1 wll)
                           lw)))
    (if (> stop-at (wall-x2 wll))
        next-pos
        stop-at)))
;------------------------------------------------------------------
;grow-wall-up : Wall NELoW ->Coordinate
;will return x coordinate of top end of wall
;Example
(begin-for-test 
  (check-equal? 
   (grow-wall-up 
    (make-wall 10 30 10 90)
    WALL-INIT)
   22
   "top coordinate of wall"))
;STRATEGY data Decomposition on wll : Wall

(define (grow-wall-up wll lw)  
  (local (
          ;Will return new coordinate without limit
          (define next-pos (- (wall-y1 wll)
                              WALL-GROW-RATE))
          ;will return boundary of wall
          (define stop-at (get-top-limit  
                           (wall-x1 wll)  
                           (+ (wall-y1 wll) WALL-GROW-RATE)  
                           lw)))
    (if (< stop-at (wall-y1 wll))
        next-pos
        stop-at)))
;------------------------------------------------------------------
;grow-wall-down : Wall NELoW ->Coordinate
;will return x coordinate of bottom end of wall
;Example
(begin-for-test 
  (check-equal? 
   (grow-wall-down
    (make-wall 10 30 10 90)
    WALL-INIT)
   98
   "bottom coordinate of wall"))
;STRATEGY data Decomposition on wll : Wall
(define (grow-wall-down wll lw)  
  (local (
          ;Will return new coordinate without limit
          (define next-pos (+ (wall-y2 wll)
                              WALL-GROW-RATE))
          ;will return boundary of wall
          (define stop-at (get-bottom-limit  
                           (wall-x1 wll)
                           (- (wall-y2 wll) WALL-GROW-RATE)
                           lw)))
    (if (> stop-at (wall-y2 wll))
        next-pos
        stop-at)))

;------------------------------------------------------------------
;check-grow : World -> Boolean
; will check if wall is growung or not 
;Example
(begin-for-test
  (check-equal?
   (check-grow INITIAL-WORLD)
   #false
   "not growing initially"))
;STRATEGY: Data Decomposition on w : World
(define (check-grow w)
  (not 
   (no-grow? 
    (world-wall w) 
    (next-wall w))))
;------------------------------------------------------------------
;no-grow? : Wall Wall -> Boolean
;Will check if both walls are equal 
;example
(begin-for-test
  (check-equal?
   (no-grow? (make-wall 10 40 10 90)
             (make-wall 10 40 10 90))
   #true
   "not growing"))
;STRATEGY: Function Composition
(define (no-grow? w1 w2)
  (equal? w1 w2))




;------------------------------------------------------------------
;get-lst-of-left-limits : Coordinate Coordinate NELoW -> ListOf<Coordinates>
; Will return sorted list of Points that is left y
;Example 
(begin-for-test 
  (check-equal? 
   (get-lst-of-left-limits 10 29 WALL-INIT)
   (list 0)
   "lower limit"))
; STRATEGY :Function Composition
(define (get-lst-of-left-limits x y low)
  (sort
   (filter  
    ; Coordinate Coordinate -> Boolean
    ; will return true if x is greater than f
    ; STRATEGY :Function Composition
    (lambda (f) (< f x)) 
    (map get-points 
         (filter 
          
          ;Wall Coordinate -> Boolean
          ;Return true if Wall is horizontal and left x
          ;STRATEGY : Data Decomposition on w : Wall
          (lambda (w) (and (eq? (wall-x1 w)(wall-x2 w))
                           (<= (wall-y1 w) y (wall-y2 w))))
          low))) >))

;------------------------------------------------------------------

;get-lst-of-right-limits : Coordinate Coordinate NELoW -> ListOf<Coordinates>
; Will return sorted list of Points that is right of x
;Example 
(begin-for-test 
  (check-equal? 
   (get-lst-of-right-limits 10 29 WALL-INIT)
   (list 400)
   "lower limit"))
; STRATEGY :Function Composition
(define (get-lst-of-right-limits x y low)
  (sort
   (filter 
    ; Coordinate Coordinate -> Boolean
    ; will return true if x is less than f
    ; STRATEGY :Function Composition
    (lambda (f) (> f x)) 
    (map get-points 
         (filter 
          ;Wall Coordinate -> Boolean
          ;Return true if Wall is horizontal and right of x
          ;STRATEGY : Data Decomposition on w : Wall
          (lambda (w) (and (eq? (wall-x1 w)(wall-x2 w))
                           (<= (wall-y1 w) y (wall-y2 w))))
          low))) <))
;------------------------------------------------------------------
;get-lst-of-top-limits : Coordinate Coordinate NELoW -> ListOf<Coordinates>
; Will return sorted list of Points that is above y
;Example 
(begin-for-test 
  (check-equal? 
   (get-lst-of-top-limits 10 29 WALL-INIT)
   (list 0)
   "lower limit"))
;STRATEGY: Data Decomposition on w: wall 
(define (get-lst-of-top-limits x y low)
  (sort
   (filter 
    ; Coordinate Coordinate -> Boolean
    ; will return true if y is greater than f
    ; STRATEGY :Function Composition
    (lambda (f) (< f y))
    (map get-points 
         (filter 
          ;Wall Coordinate -> Boolean
          ;Return true if Wall is horizontal and above x
          ;STRATEGY : Data Decomposition on w : Wall
          (lambda (w) (and (eq? (wall-y1 w)(wall-y2 w))
                           (<= (wall-x1 w) x (wall-x2 w))))  
          low))) >))


;------------------------------------------------------------------
;get-lst-of-bottom-limits : Coordinate Coordinate NELoW -> ListOf<Coordinates>
; Will return sorted list of Points that is below y
;Example 
(begin-for-test 
  (check-equal? 
   (get-lst-of-bottom-limits 10 29 WALL-INIT)
   (list 400)
   "lower limit"))
;STRATEGY: Data Decomposition on w: wall   
(define (get-lst-of-bottom-limits x y low)
  (sort
   (filter 
    ; Coordinate Coordinate -> Boolean
    ; will return true if y is less than f
    ; STRATEGY :Function Composition
    (lambda (f) (> f y ))
    (map get-points 
         (filter 
          ;Wall Coordinate -> Boolean
          ;Return true if Wall is horizontal and below x
          ;STRATEGY : Data Decomposition on w : Wall
          (lambda (w) (and (eq? (wall-y1 w)(wall-y2 w))
                           (<= (wall-x1 w) x (wall-x2 w)))) 
          low))) <))    



;------------------------------------------------------------------
;get-top-limit : Coordinate Coordiante NELoW -> Coordinate
; will return nearest top boundary of wall
;example
(begin-for-test
  (check-equal?
   (get-top-limit 49 90 WALL-INIT)
   0
   "top boundary"))
;STRATEGY: Function Composition

(define (get-top-limit x y low)
  (get-first-list (get-lst-of-top-limits x y low)))
;------------------------------------------------------------------
;get-bottom-limit : Coordinate Coordiante NELoW -> Coordinate
; will return nearest bottom boundary of wall
;example
(begin-for-test
  (check-equal?
   (get-bottom-limit 49 90 WALL-INIT)
   400
   "bottom boundary"))
;STRATEGY: Function Composition
(define (get-bottom-limit x y low)
  (get-first-list(get-lst-of-bottom-limits x y low)))
;------------------------------------------------------------------
;get-left-limit : Coordinate Coordiante NELoW -> Coordinate
; will return nearest left boundary of wall
;example
(begin-for-test
  (check-equal?
   (get-left-limit 49 90 WALL-INIT)
   0
   "left boundary"))
;STRATEGY: Function Composition
(define (get-left-limit x y low)
  (get-first-list(get-lst-of-left-limits x y  low)))
;------------------------------------------------------------------
;get-right-limit : Coordinate Coordiante NELoW -> Coordinate
; will return nearest right boundary of wall
;example
(begin-for-test
  (check-equal?
   (get-right-limit 49 90 WALL-INIT)
   400
   "top boundary"))
;STRATEGY: Function Composition
(define (get-right-limit x y low)
  (get-first-list(get-lst-of-right-limits x y low)))
;------------------------------------------------------------------
;get-points : Wall -> Coordinate
;Will return minor coordinate of a wall ( which is same throughtout wall)
(begin-for-test
  (check-equal?
   (get-points (make-wall 10 40 20 40 ))
   40
   "y corrdinate")
  (check-equal?
   (get-points (make-wall 10 40 10 50))
   10
   "x coordinate"))
;STRATEGY: Data Decomposition on wll: Wall
(define (get-points wll)
  (if(eq? (wall-x1 wll) (wall-x2 wll))
     (wall-x1 wll)
     (wall-y1 wll)))

;------------------------------------------------------------------------

; next-balls : NELoB NELoW-> NELoB
; Computes next position of center of each balls
; Example 
(begin-for-test
  (check-equal?  (next-balls LV2-BALL-LIST WALL-INIT)
                 (list (make-ball 36 44 8 7) (make-ball 34 62 2 5))
                 "List of balls at next position"))
;STRATEGY : Function Composition 
(define (next-balls lb lw)
  (map ( lambda (x) (next-ball x lw)) lb))

;--------------------------------------------------------------------------
;next-ball: Ball NELoW-> Ball  
;Computes the position of next ball on canvas
;Example:
(begin-for-test
  (check-equal?  (next-ball (make-ball 28 37 8 7  ) WALL-INIT)
                 (make-ball 36 44 8 7  )
                 "position and velocity of next ball"))
; STRATEGY: Data Decomposition on b : Ball

(define (next-ball b lw)
  (make-ball (new-ball-x 
              (ball-x b) 
              (ball-vx b) 
              ( + (get-left-limit (ball-x b )(ball-y b) lw) 20)  
              (- (get-right-limit (ball-x b )(ball-y b) lw) 20))
             (new-ball-y 
              (ball-y b) 
              (ball-vy b) 
              ( + (get-top-limit (ball-x b )(ball-y b) lw) 20) 
              ( -(get-bottom-limit (ball-x b )(ball-y b) lw) 20))
             (new-vel-x 
              (ball-x b)
              (ball-vx b)
              ( + (get-left-limit (ball-x b )(ball-y b) lw) 20)  
              (- (get-right-limit (ball-x b )(ball-y b) lw) 20)) 
             (new-vel-y 
              (ball-y b) 
              (ball-vy b)
              ( + (get-top-limit (ball-x b )(ball-y b) lw) 20) 
              ( -(get-bottom-limit (ball-x b )(ball-y b) lw) 20))))

;--------------------------------------------------------------------------
;new-ball-x : Coordinate Integer -> Coordinate
;Computes new x position of center of ball
;Example:
(begin-for-test
  (check-equal?
   (new-ball-x 20 -5 20 40 )
   20
   "new ball x"))
;STRATEGY: Function Composition
(define (new-ball-x x vx mn mx)
  (if (check-x? x vx mn mx)
      (+ x vx)
      (ball-hr-edge vx mn mx)))
;--------------------------------------------------------------------------
;new-ball-y : Coordinate Integer -> Coordinate
;Computes new y position of center of ball
;Example:
(begin-for-test
  (check-equal?
   (new-ball-y 20 -5 20 40 )
   20
   "new ball y"))
;STRATEGY: Function Composition
(define (new-ball-y y vy mn mx)
  (if (check-y? y vy mn mx)
      (+ y vy)
      (ball-vr-edge vy mn mx)))
;--------------------------------------------------------------------------
;new-vel-x : Coordinate Integer -> Integer
;Computes new horizontal Velocity of ball
;Example:
(begin-for-test
  (check-equal?
   (new-vel-x 20 -5 20 40 )
   5
   "new vel x"))
;STRATEGY: Function Composition
(define (new-vel-x x vx mn mx)
  (if(check-x? x vx mn mx)
     vx
     (* -1 vx)))
;--------------------------------------------------------------------------
;new-vel-y : Coordinate Integer -> Integer
;Computes new vertical Velocity of ball
;Example:
(begin-for-test
  (check-equal?
   (new-vel-y 20 -5 20 40 )
   5
   "new vel y"))
;STRATEGY: Function Composition
(define (new-vel-y y vy mn mx)
  (if(check-y? y vy mn mx)
     vy
     (* -1 vy)))
;--------------------------------------------------------------------------
;check-x? : Coordinate Integer -> Boolean
;Checks whether new x position is within limits
;Example:
(begin-for-test
  (check-equal?
   (check-x? 20 -5 20 40 )
   #f
   "check x"))
;STRATEGY: Function Composition
(define (check-x? x vx mn mx)
  (<= mn (+ x vx) mx))
;--------------------------------------------------------------------------
;check-y? : Coordinate Integer -> Boolean
;Checks whether new y position is within limits
;Example:
(begin-for-test
  (check-equal?
   (check-y? 20 -5 20 40 )
   #f
   "check y"))
;STRATEGY: Function Composition
(define (check-y? y vy mn mx)
  (<= mn (+ y vy) mx))
;--------------------------------------------------------------------------
;ball-hr-edge : Integer -> Coordinate 
;Will move the ball to be flushed with left or right wall
;Example:
(begin-for-test
  (check-equal?
   (ball-hr-edge  5 20 40 )
   40
   "check x max"))
;STRATEGY: Function Composition
(define (ball-hr-edge vx mn mx)
  (if (> vx 0)
      mx
      mn))
;--------------------------------------------------------------------------
;ball-vr-edge : Integer -> Coordinate 
;Will move the ball to be flushed with top or bottom wall
;Example:
(begin-for-test
  (check-equal?
   (ball-vr-edge  5 20 40 )
   40
   "check y max"))
;STRATEGY: Function Composition
(define (ball-vr-edge vy mn mx)
  (if (> vy 0)
      mx
      mn))



;------------------------------------------------------------------------
;render-last: World -> Image
;will return end of world image
;example
(begin-for-test 
  (check-equal? (render-last INITIAL-WORLD)
                (place-image END-IMAGE CENTER-X CENTER-Y 
                             (render INITIAL-WORLD))
                "last scene"))
;STRATEGY: Function Composition 
(define (render-last w)
  (place-image END-IMAGE CENTER-X CENTER-Y (render w)))

;------------------------------------------------------------------------
;end? World -> Boolean
; will retunr if the game is over
;example
(begin-for-test 
  (check-equal? (end? INITIAL-WORLD)
                #false
                "game-over"))
;STRATEGY: Data Decomposition on w: World 
(define (end? w)
  #false )

;=====================================================================
  
;ALTERNATE DATA DEFINITION

; 1
 ;A World is a 
;(make-world (NELoB NELoW Natural Wall Boolean PosInt)
; represents a World of where balls in list of ball are moving with 
;random velocities 

;(define-struct world ( LoB score wall axis level grow))

;**
; A Ball is a (make-ball Coordinate Coordinate Integer Integer Coordinate
;coordinate coordinate coordinate) 
; represents a ball that moves around in the region determined by xmin ymin 
;xmax ymax.

;(define-struct ball( x y vx vy xmin ymin xmax ymax))
;NELoB , KeyEvent, Axis, MouseHandler , Wall is same as above. 

;Pros : No need to consider List of walls . Here we are dividing the region 
;    that is stored in each ball and hence each ball will move 
;    around its region
;Cons : Rendering will be difficult since we need to tak out regions of 
;    each ball and place a wall between an image.
;     regions are not a property of wall and if the area changes to 
;      other shape  rather than square or rectangle then this case will fail



;
;2: 

;A World is a 
;(make-world (NELoB NELoW Natural Wall Boolean PosInt)
; represents a World of where balls in list of ball are moving with 
;random velocities 

;(define-struct world ( LoB score wall axis level grow))

;**
;A Wall is a (make-wall ( Axis  Coordinate Coordiante Integer Integer )
; represents a wall that is created on every click .
; INTERP: Wall are either parallel or perpendicular to x-axis
;        Coordiante x y are the point on the wall from where it originated
;      h-end , l-end will be the point where wall will stop 
;(define-struct wall( Axis x y  h-end l-end ))

;NELoB, NELoW , KeyEvent, Axis, MouseHandler , Ball is same as above. 

;Pros: 
;   -No need to check for the axis before creating wall
;   -only the coordinate that will change is considered to exceed or 
;   decrease till h-end or l-end is reached.
;   -In (make-wall x1 y1 x2 y2) either x1 x2 is same or y1 y2 is same.
;    this is not the case here


;CONS:
;  - keeping axis on both World (for growing wall) and each wall is redundant
;  - rendering is very easy we have all the end points of a wall
;  - each time we need to add h-end and l- end to the x or y components while 
;   checking collisions aor growing end of wall
;   - when Wall is growing there is check on two Axis one is of world and 
;   other is its own . Both need to be made equal everytime a valid mouse
;    event happens
;   its easy to determin axis by comparing end points. Another component for 
;   axis is not needed
