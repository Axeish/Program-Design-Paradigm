;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bounce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define  TIME-ON-TASK 65)

(check-location "03" "bounce.rkt")

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide mouse-handler)
(provide world-ball)
(provide world-paused?)
(provide ticks-since-click)
(provide score)
(provide ball-x)
(provide ball-y)

;canvas constants
(define CENTER-X 150)                  ;pixels
(define CENTER-Y 200)                  ;pixels
(define WIDTH (* 2 CENTER-X))          ;pixels
(define HEIGHT (* 2 CENTER-Y))         ;pixels
(define MTS (empty-scene WIDTH HEIGHT)); image
(define TEXTCOLOR "black")
(define TEXTSIZE 25)
(define PAUSE-IMG (text "Paused" TEXTSIZE TEXTCOLOR)); image
(define SCORE-IMG (text "Score  " TEXTSIZE TEXTCOLOR)); image

;ball constants
(define BALL-RADIUS 20 )               ;pixels
(define VELOCITY 3)                    ;Integer
(define TIME 1)                        ;Number
(define ACCELERATION 1)                ;Number
(define HALF 0.5)                      ;Number
(define VEL-COEFF 0.9)                 ;Number
(define INITIAL-PAUSED? #false)        ;Number
(define ZERO 0)                        ;Zero
(define BALL-IMG (circle BALL-RADIUS "solid" "black"));image

;other constant;
(define BALL-RIGHT-EDGE (- WIDTH BALL-RADIUS))
(define BALL-LEFT-EDGE BALL-RADIUS)
(define BALL-TOP-EDGE  BALL-RADIUS)
(define BALL-BOTTOM-EDGE (- HEIGHT BALL-RADIUS))
(define ε 0.001)


;----------------Data Definitions-----------------------------------

;A Pixels is a NonNegReal, is a distance on canvas.

;X-Dir is one of the:
; - "right"
; - "left"
; INTERP :  represents a canvas direction on horizontal direction.
(define RIGHT "right")
(define LEFT "left")

;<X-Dir predicates> :X-Dir -> Boolean
;Returns true if d is the Direction indicated by the function name.
;STrategy:Data Decomposition on dir: X-Dir
(define (right? d) (string=? d RIGHT))
(define (left? d)  (string=? d LEFT))

; TEMPLATE
; x-dir-fn: X-Dir -> ???
;(define (x-dir-fn dir)
;  (cond
;    [(right? dir) ...]
;    [(left? dir) ...]))


;Y-Dir is one of the:
; - "up"
; - "down"
; INTERP :  represents a canvas direction where "down" is y-increasing
(define UP "up")
(define DOWN "down")

; <Y-Dir predicates> : Y-Dir -> Boolean
; Returns true if d is the Direction indicated by the function name.
; STRATEGY: Data Decomposition on d: Y-Dir
(define (up? d)   (string=? d UP))
(define (down? d) (string=? d DOWN))

; TEMPLATE
; y-dir-fn : Y-Dir -> ???
;(define (y-dir-fn dir)
;  (cond
;    [(up? dir) ...]
;    [(down? dir) ...]))


;-----------------Ball Definition ----------------------------------------
;A Ball is a 
;(make-ball Coordinate Coordinate velocity X-dir Y-dir PosInteger)
;INTERP : 
;x, y are X-coordinate and Y-Coordinate of ball
;counter is the time since the explosion 
;vel is the velocity by which the y coordinate of ball will change 
(define-struct ball ( x y vel xdir ydir counter))

;Examples:
(define INITIAL-BALL (make-ball 150 20 0 RIGHT DOWN 0))
(define BALL1 (make-ball 150 150 10 LEFT UP 0))
(define BALL2 (make-ball 20 230 3 LEFT DOWN 0))
(define BALL3 (make-ball 20 230 3 LEFT DOWN 3))
(define BALL4 (make-ball 120 230 3 RIGHT DOWN 3))
(define BALL-AT-FLOOR (make-ball 120 380 3 RIGHT DOWN 3))

; TEMPLATE:
; ball-fn : Ball -> ???

(define (ball-fn b)
  (... (ball-x b) ...(ball-y b) ... (ball-vel b)...
       (x-dir-fn (ball-xdir b))...
       (y-dir-fn (ball-ydir b))...
       (ball-counter b)))


; ------------------World Definition ---------------------------------
; A World is a (make-world Ball posNumber Boolean)
; INTERP :
; total score is the count of total number of correct click on balls
; paused? will tell if the state of worlds is paused or not 
(define-struct world ( ball totalscore paused?))

;Examples:
(define INITIAL-WORLD( make-world INITIAL-BALL ZERO INITIAL-PAUSED?))
(define WORLD-MID ( make-world BALL1 ZERO INITIAL-PAUSED?))
(define WORLD-MID-PAUSED ( make-world BALL1 ZERO #true))
(define WORLD-EXPLODE ( make-world BALL3 ZERO #false))
(define WORLD-GOOD-SCORE ( make-world BALL1 20 #false))
(define WORLD-GAMEOVER ( make-world BALL-AT-FLOOR 20 #false))

; TEMPLATE:
; world-fn : World -> ???
(define (world-fn w)
  (... (world-ball w) ... (world-totalscore w) ... (world-paused? w) ...))
  
  
;-------------------Stimulation handler---------------------------
;run :World -> World
;Starts the Stimulation
;STRATEGY: Function Composition
(define  (run world)
  (big-bang world
            (to-draw draw-world)
            (on-tick next-world)
            (on-key key-handler)
            (on-mouse mouse-handler)))


;------------------Functions ---------------------------
;---------------Draw-world------------------------------

;draw-world :World -> image
;renders the current world state w
;Example
(begin-for-test
  (check-equal? (draw-world WORLD-MID-PAUSED) 
                (place-image PAUSE-IMG 50 50 
                    (place-image BALL-IMG 150 150
                        (place-image 
                         (beside 
                          SCORE-IMG
                                 (text "0" 30 "black")) 220 50 MTS)))
                "image is drawn"))
;STRATEGY : Data Decomposition of w : World
(define (draw-world w)
 (if (world-paused? w)
     (place-image PAUSE-IMG 50 50 
                  (draw-ball-on (world-ball w)
                               (render-scene (world-totalscore w))))
  (draw-ball-on (world-ball w) 
               (render-scene (world-totalscore w)))))
;Testing
(begin-for-test
  (check-equal? (draw-world WORLD-MID-PAUSED) 
                (place-image PAUSE-IMG 50 50 
                    (place-image BALL-IMG 150 150
                        (place-image 
                         (beside 
                          SCORE-IMG
                                 (text "0" 30 "black")) 220 50 MTS))) 
                "testing image render :paused")
  (check-equal? (draw-world WORLD-MID) 
                (place-image BALL-IMG 150 150 
                    (place-image 
                     (beside 
                      SCORE-IMG 
                      (text "0" 30 "black")) 220 50 MTS))
                "testing image render: normal ")
  
  (check-equal? (draw-world WORLD-EXPLODE) 
                (place-image (overlay/align "middle" "middle"
                              (radial-star 12  14 
                                              28
                                              "solid" "yellow") 
                              BALL-IMG) 20 230 
                    (place-image
                     (beside 
                      SCORE-IMG
                      (text "0" 30 "black")) 220 50 MTS))
                "testing image render:exploded"))


;render-scene : Score -> Image
;renders the current world state w
;Example
(begin-for-test
  (check-equal? (render-scene 23 )
                (place-image 
                 (beside
                  SCORE-IMG
                  (text "23" 30 TEXTCOLOR)
                  ) 220 50 MTS)
                "render score 23 "))
                
;STRATEGY : Function Composition
(define (render-scene score )
    (place-image 
   (beside
         SCORE-IMG
         (text (number->string score) 30 TEXTCOLOR)) 220 50 MTS))


; draw-ball-on : Ball Image -> Image
; Draws Ball b onto Image img.
; WHERE: Ball image completely inside the img
;Example
(begin-for-test
  (check-equal? (draw-ball-on BALL1 MTS)
                (place-image BALL-IMG 150 150 MTS)
                "image of a ball on screen"))
; STRATEGY: data decomposition on b : Ball
(define (draw-ball-on b img )
  (if (>(ball-counter b) 0)
      (place-image
      (overlay/align "middle" "middle"
                              (radial-star 12  (* 2 (- 10 (ball-counter b))) 
                                              (* 4 (- 10 (ball-counter b)))
                                              "solid" "yellow") 
                              BALL-IMG) (ball-x b) (ball-y b) img)
  (place-image BALL-IMG (ball-x b) (ball-y b) img)))

;-------------------------next-world---------------------------------------

;next-world :World ->World
;computes the next world state from given world state
;Example
(begin-for-test
  (check-equal? (next-world WORLD-MID-PAUSED) 
                (make-world (make-ball 150 150 10 "left" "up" 0) 0 true)
                "next State of world"))
;STRATEGY: Data decomposition on w: World
(define (next-world w)
  (if (world-paused?  w) 
      w 
      (make-world (next-ball (world-ball w)) (score w) #false)))

;Testing
(begin-for-test
  (check-equal? (next-world WORLD-MID) 
                (make-world (make-ball 147 140.5 9 "left" "up" 0) 0 false)
                "next State of WORLD-MID")
  (check-equal? (next-world WORLD-MID-PAUSED) 
                (make-world (make-ball 150 150 10 "left" "up" 0) 0 true)
                "next State of WORLD-MID-PAUSED")
  (check-equal? (next-world WORLD-EXPLODE) 
                (make-world (make-ball 20 234.5 4 "right" "down" 2) 0 false)
                "next State of WORLD-EXPLODE"))
  

;next-ball: Ball-> Ball  
;Computes the position of next ball on canvas
;Example:
(begin-for-test
  (check-equal? (next-ball INITIAL-BALL)
                (make-ball 153 21.5 1 "right" "down" 0)
                "movement of inital ball"))
; STRATEGY: function composition
(define (next-ball b)
  (if (ball-off-canvas? (ball-forward b))
      (ball-to-edge b)
      (ball-forward b))) 
;Testing
(begin-for-test
  (check-equal? (next-ball INITIAL-BALL)
                (make-ball 153 21.5 1 "right" "down" 0)
                "movement of inital ball")
  (check-equal?  (next-ball (make-ball 67 353.66 0.17 "left" "up" 0))
                 (make-ball 64 353.99 -0.83 "left" "down" 0)
                 "change in direction from UP to Down")
  (check-equal?  (next-ball (make-ball 20 353.66 0.17 "left" "up" 0))
                 (make-ball 20 353.99 -0.83 "right" "up" 0)
                 "change in direction from Left to Right")
  (check-equal?  (next-ball (make-ball 280 280 0.17 "right" "up" 0))
                 (make-ball 280 280.33 -0.83 "left" "up" 0)
                 "change in direction from Right to Left")
  (check-equal?  (next-ball(make-ball 80 380 7 "left" "down" 0))
                 (make-ball 77 380 36/5 "left" "up" 0)
                 "change in direction from Bottom  to up"))
  
; ball-forward : Ball -> Ball
; Move ball b by VELOCITY pixels in horizontal & by vel in vertical drection.
;Example
(begin-for-test
  (check-equal? (ball-forward BALL2)
                (make-ball 17 234.5 4 "left" "down" 0)
                "new ball at ne position"))
; STRATEGY: data decomposition of b :Ball
(define (ball-forward b)
  (make-ball
   (x-forward (ball-x b) (ball-xdir b))
   (y-forward (ball-y b) (ball-ydir b) (ball-vel b)) 
   (calc-y-velocity (ball-vel b) (ball-ydir b) )
   (ball-xdir b)
   (change-dir b)
   (check-counter (ball-counter b) (ball-y b))
   ))

; change-dir : Ball -> Y-Dir
; Change the direction of Ball from top up to down if it reaches highest point
; WHERE: Y-dir is only UP
;Example
(begin-for-test
  (check-equal? (change-dir (make-ball 67 353.66 0.17 "left" "up" 0))
                DOWN
                "change dir from up to down"))
; STRATEGY : Data Decomposition on b : Ball
(define (change-dir b)
  (if (and (>=(ball-vel b) 0) 
           (< (calc-y-velocity (ball-vel b) (ball-ydir b) ) 0))
      DOWN
  (ball-ydir b)))

; x-forward : Coordinate X-Dir -> Ball
; Move coordinate x by VELOCITY pixels in direction dir.
;Example
(begin-for-test
  (check-equal? (x-forward 23 LEFT)
                20 
                "move left by 3 pixel"))
; STRATEGY: Data Decomposition on dir: X-Dir
(define (x-forward x dir )
  (cond
    [(left? dir) (- x VELOCITY)]
    [(right? dir) (+ x VELOCITY)]))

; y-forward : Coordinate Y-Dir -> Ball
; Move coordinate y by VELOCITY pixels in direction dir.
;Example
(begin-for-test
  (check-equal? (y-forward 20 UP 10)
                10.5
                "move  up by 10.5 pixels"))
; STRATEGY: data decomposition on dir : Direction
(define (y-forward y dir vel)
  (cond
    [(up? dir) (- y  (calc-y-distance vel dir ))]
    [(down? dir) (+ y (calc-y-distance vel dir ) )]))


;calc-y-distance : Integer Y-Dir ->Integer
;calculate change in y direction  of ball in up/down direction
;Example
(begin-for-test
  (check-equal? (calc-y-distance 10 DOWN)
                11.5 
                "change in y of b"))
;STRATEGY : Function Composition
(define (calc-y-distance vel dir )
  (+ (calc-y-velocity vel dir) (* HALF ACCELERATION TIME)))


;calc-y-velocity : Integer Y-Dir ->Integer
; calculate new Velocity of ball in up direction
;Example
(begin-for-test
  (check-equal? ( calc-y-velocity  10 DOWN) 11 "new y-velocity"))
;STRATEGY : Data Decomposition on dir: Y-Dir
(define (calc-y-velocity vel dir )
  (cond 
    [(up? dir )(-  vel (* ACCELERATION TIME))]
    [(down? dir )(+ vel (* ACCELERATION TIME))]))

; ball-off-x-canvas? : Ball -> Boolean
; Returns true if any part of b is at or past the left
;or right canvas edge.
;Example
(begin-for-test
  (check-false (ball-off-x-canvas? BALL1)
              "ball is inside "))  
; STRATEGY: function composition
(define (ball-off-x-canvas? b)
  (or (ball-at/past-left-edge? b)
      (ball-at/past-right-edge? b)))

; ball-off-y-canvas? : Ball -> Boolean
; Returns true if any part of b is at or past the up or down canvas edge.
;Example
(begin-for-test
  (check-false (ball-off-y-canvas? BALL1)
              "ball is inside "))
; STRATEGY: function composition
(define (ball-off-y-canvas? b)
   (ball-at/past-bottom-edge? b))


; ball-off-canvas? : Boolean -> Boolean
; Returns true if any part of b is at or past the 
;left or right or up or down canvas edge.
;Example
(begin-for-test
  (check-false (ball-off-canvas? BALL1)
              "ball is inside "))
;STRATEGY: Function composition
(define (ball-off-canvas? b)
  (or (ball-off-x-canvas? b)
      (ball-off-y-canvas? b)))

; <Ball edge predicates> : Ball -> Boolean
; Returns true if any part of b is at or past the named edge.

;Example
(begin-for-test
  (check-false (ball-at/past-left-edge? BALL1)
               "ball is inside"))

; STRATEGY: data decomposition on b : Ball
(define (ball-at/past-left-edge? b)
  (<= (ball-x b) BALL-LEFT-EDGE))
(define (ball-at/past-right-edge? b)
  (>= (ball-x b) BALL-RIGHT-EDGE))
(define (ball-at/past-bottom-edge? b)
  (>= (ball-y b) BALL-BOTTOM-EDGE))



; ball-to-edge : Ball -> Ball
; Moves b to the furthest edge in the direction it's facing.
;Example
(begin-for-test 
  (check-equal? (ball-to-edge BALL3)
                (make-ball 20 234.5 4 "right" "down" 2)
                "ball at left edge"))
; STRATEGY: data decomposition on b : Ball
(define (ball-to-edge b)
  (make-ball
   (check-hr-edge b)
   (check-vr-edge b)
   (calc-edge-velocity b)
   (check-hr-dir  b)
   (check-vr-dir  b)
   (check-counter (ball-counter b) (ball-y b))))

;calc-edge-velocity: Ball -> Number
;Will return the velocity of the ball .
;Example
(begin-for-test 
  (check-equal? (calc-edge-velocity BALL1)
                9
                "velocity of ball in y-dir"))
;STRATEGY : Data Decomposition on b: BAll
(define (calc-edge-velocity b)
  (if (ball-off-y-canvas? (ball-forward b)) 
      (* VEL-COEFF (calc-y-velocity (ball-vel b) (ball-ydir b)))
      (calc-y-velocity (ball-vel b) (ball-ydir b))
      ))
  
; (calc-y-velocity (ball-vel b) (ball-ydir b)))
  
; check-hr-edge : Ball -> Coordinate
; returns x coordinate of ball 
;Example
(begin-for-test 
  (check-equal? (check-hr-edge BALL1)
                147 "x-coordinate"))
;STRATEGY : Function Composition
(define (check-hr-edge b)
  (if (ball-off-x-canvas? (ball-forward b)) 
      (move-x-edge (ball-xdir b))
      (ball-x (ball-forward b))
      ))
;move-x-edge : X-Dir -> Coordinate
;moves ball to left or right edge if it is trying to bo beyond wall
;Example
(begin-for-test 
  (check-equal? (move-x-edge LEFT)
                20
                "right edge "))
;STRATEGY: Data Decomposition of dir : X-Dir
(define (move-x-edge dir )     
  (cond
    [(left? dir) BALL-LEFT-EDGE ]
    [(right? dir) BALL-RIGHT-EDGE ]))

;Check-vr-edge : Ball -> Coordinate
;moves ball to the floor ofthe room if it is tring to go beyond floor
;Example
(begin-for-test 
  (check-equal? (check-vr-edge BALL2)
                234.5
                "Y-Coordinate of ball"))

;STRATEGY :Function Composition
(define (check-vr-edge b)
  (if (ball-off-y-canvas? (ball-forward b)) 
      BALL-BOTTOM-EDGE
      (ball-y (ball-forward b))
      ))

;check-vr-dir-b : Ball -> Y-Dir
;returns direction of ball vertically
;Example
(begin-for-test 
  (check-equal? (check-vr-dir BALL2)
                "down"
                "y-dir of ball"))
;STRATEGY: Function Composition
(define (check-vr-dir b)
  (if (ball-off-y-canvas? (ball-forward b)) 
      UP
      (ball-ydir b)
      ))

;check-hr-dir : Ball -> X-Dir
;will return horizontal direction of ball movement
;Example
(begin-for-test 
  (check-equal? (check-hr-dir BALL2)
                "right" "X-Dir of ball"))

;STRATEGY : Function Composition
 
(define (check-hr-dir b)
  (if (ball-off-x-canvas? (ball-forward b)) 
      (move-x-edge-dir (ball-xdir b))
      (ball-xdir b)
      ))

;move-x-edge-dir : X-Dir ->X-Dir
;Will return reverse of horizontal direction of ball movement
;Example
(begin-for-test 
  (check-equal? (move-x-edge-dir LEFT) RIGHT
                "LEFT->RIGHT"))
  
;STRATEGY: Data Decomposition of dir : X-Dir
(define (move-x-edge-dir dir )     
  (cond
    [(left? dir) RIGHT ]
    [(right? dir) LEFT ]))


;----------------------------Key Handler-----------------------------

; key-handler : World KeyEvent -> World
; Computes the next world after a key press.
;Example
(begin-for-test 
  (check-equal? (key-handler WORLD-MID-PAUSED "p")
                WORLD-MID
                "unpaused state"))
;STRATEGY : Function Composition
(define (key-handler w key)
  (cond [
  (key=? key "p") (pause-ball w)]
        [else w]))

;Testing
(begin-for-test 
  (check-equal? (key-handler WORLD-MID "p")
                WORLD-MID-PAUSED 
                "paused state")
  (check-equal? (key-handler WORLD-MID-PAUSED "p")
                WORLD-MID
                "unpaused state")
  (check-equal? (key-handler WORLD-MID "k")
                WORLD-MID 
                "invalid key "))
; pause-ball : World -> World
; makes a new World and update the pause state of the world
;Example
(begin-for-test 
  (check-equal? (pause-ball WORLD-MID)
                (make-world (make-ball 150 150 10 "left" "up" 0) 0 true)
                "Paused"))
;STRATEGY : Data Decomposition on w : World
(define (pause-ball w)
   (make-world (world-ball w) (score w) 
               ( change-state (world-paused? w))))

; change-pause-state : Boolean -> Boolean
; will change the pause state of the world
;Example
(begin-for-test
  (check-true (change-state  #false)
              "change state ")) 
;STRATEGY : function composition
(define (change-state p)
 (not p))


; ticks-since-click : World -> [0 10)
; Returns the number of ticks since the last explosion, if there's
; currently one. 0 means no explosion.
;Example
(begin-for-test
  (check-equal? (ticks-since-click WORLD-MID)
                0
                "no explosion"))
;STRATEGY: Data Decomposition on w: World
(define (ticks-since-click w)
   (ball-counter (world-ball w)))

;Testing
(begin-for-test
  (check-equal? (ticks-since-click INITIAL-WORLD)
                0 
                "no explosion yet ")
  (check-equal? (ticks-since-click WORLD-EXPLODE)
                3 
                "Tick after explosion"))

;score: World -> nonNegInteger
;Return the total score of the world
;Example
(begin-for-test
  (check-equal? (score WORLD-GOOD-SCORE ) 20 "score 20"))
;STRATEGY : Data Decomposition on w: WORLD
(define (score w)
  (if  (check-floor-touched (world-ball w)) 0
  (world-totalscore w)))

;Testing
(begin-for-test
  (check-equal? (score INITIAL-WORLD ) 0 "initial zero score")
  (check-equal? (score WORLD-GOOD-SCORE ) 20 "the  score  is 20")
(check-equal? (score WORLD-GAMEOVER ) 0 "game over"))

;Check-floor-touched?: Ball -> Boolean
;will return if the ball has touched the bottom edge or not
;Example
(begin-for-test 
  (check-false (check-floor-touched BALL1)
               "ball not touching the floor"))
;STRATEGY : Data Decomposition on b :Ball
(define (check-floor-touched b)
  (eq? ( ball-y b) BALL-BOTTOM-EDGE ))

;-----------------------Mouse Handler---------------------------------------


;mouse-handler: World Coordinate Coordinate MouseEvent -> World
;will return new world based on mouse click
;Example
(begin-for-test 
  (check-equal? (mouse-handler WORLD-MID 145 145 "button-down")
                (make-world (make-ball 150 150 20 "left" "up" 10) 1 false)
                "ball is clicked"))
;STRATEGY: Function Composition
(define (mouse-handler w x y mev)
  (cond
    [(and (mouse=? mev "button-down") (not (world-paused? w)))
     (world-after-click w x y)]
    [else w]))

;Testing
(begin-for-test
  (check-equal? (mouse-handler WORLD-MID 150 150 "button-down")
                (make-world (make-ball 150 150 20 "left" "up" 10) 1 false)
                "ball is clicked at center ")
  (check-equal? (mouse-handler WORLD-MID 145 145 "button-down")
                (make-world (make-ball 150 150 20 "left" "up" 10) 1 false)
                "ball is clicked not at center ")
  (check-equal? (mouse-handler WORLD-MID 150 150 "button-up")
                WORLD-MID
                "invalid mouse event")
  (check-equal? (mouse-handler WORLD-MID 15 15 "button-down")
                WORLD-MID
                "ball not clicked")
  (check-equal? (mouse-handler INITIAL-WORLD 160 20 "button-down")
                (make-world (make-ball 150 20 -10 "right" "down" 10) 
                            1 
                            false)
                "ball not clicked"))
                
;world-after-click:  World Coordinate Coordinate -> World
;Will return new world if ball is clicked
;Example
(begin-for-test
  (check-equal? (world-after-click WORLD-MID 150 150)
                (make-world (make-ball 150 150 20 "left" "up" 10) 1 false)
                "if in-ball then sent to explosion function"))
;STRATEGY : Function Composition
(define (world-after-click w x y)
  (if (in-ball? (world-ball w) x y )
      (make-world (explode-ball(world-ball w)) 
         (add1 (score w)) (world-paused? w))
      w))


  
;Explode-vel: Integer Y-Dir -> Integer
; will return new velocity of the ball after it is hit 
;Example
(begin-for-test
  (check-equal? (explode-vel 20 "up")
                30
                "add 10 if dir is up"))
; STRATEGY : Data Decomposition on dir: Y-Dir

(define (explode-vel vel dir)
  (cond
    [(up? dir) (+ vel 10)]
    [else -10]))



;explode-ball: Ball-> Ball
; return a new ball with new velocity once explosion starts
;Example
( begin-for-test
  ( check-equal? ( explode-ball BALL1)
                ( make-ball 150 150 20 "left" "up" 10)
                "add 10 seconds for counter"))
                
;STRATEGY :Data Decomposition on b : Ball
( define ( explode-ball b)
  (  make-ball
   (  ball-x b)
   (  ball-y b)
   (  explode-vel (  ball-vel b) (  ball-ydir b))
   (  ball-xdir b)
   ( ball-ydir b)
   10))

;in-ball? : Ball Coordinate Coordinate -> Boolean
;Will check if the click position falls on space occupied by ball
;Example
( begin-for-test
  ( check-true (  in-ball? BALL1 150 150)
              "coordinate inside ball"))
;STRATEGY: data decomposition of b: Ball
(  define (  in-ball? b x y)
  ( > (sqr BALL-RADIUS) 
      (+ (sqr (- x (ball-x b))) 
         (sqr (- y (ball-y b))))))


;check-counter : nonNegInteger Coordinate-> nonNegInteger
; will return the time after explosion
;Example
( begin-for-test
  ( check-equal? ( check-counter 4 6)
                3
                "t>0 and y<380 so -1 "))
;STRATEGY: Function Composition
(define (check-counter t y)
  ( if(and (> t 0) (< y BALL-BOTTOM-EDGE)) 
      (sub1 t) 
      0))

;------------Rounding function--------------------------------------

; round/ε : Real -> Real
; Rounds x to within ε precision
;Example   
(begin-for-test
  (check-equal? (round/ε .960596059605960509650965)
                #i0.961
                "round off"))
;STRATEGY: Function Composition
(define (round/ε x)
  (exact->inexact (* (inexact->exact (round (/ x ε))) ε)))


;(run INITIAL-WORLD)


;-----------ALTERNATE DATA DEFINITION------------------------------------
;-------------------------------------------------------------------------
;1. Alternate Data Definition
;
;A World is a (make-world Coordinate Coordinate 
;                  Number x-dir y-dir 
;                  posInteger 
;                   Boolean PosNumber)
;
;INTERP: ball-x and ball-y are the coordinates of ball 
;ball-vel is the velocity of the ball 
;ball-xdir ball-ydir are direction of the ball
;counter will store the number of ticks after explode
;paused? will check if the state of the world is paused or not
;total score will tell the number of clicks before the ball touches floor
;
;(define-struct world ( ball-x ball-y ball-vel ball-xdir ball-ydir
;                              counter  paused? totalscore ))
;TEMPLATE:
;(define (world-fn w )
;  (... (world-ball-x w) ... (world-ball-y w) ... (world-ball-vel w)
;       ... (x-dir-fn (world-ball-xdir) ) 
;       ... (y-dir-fn (world-ball-ydir) )
;       ... (world-counter w) ... (world-paused? w) 
;       ... (world-totalscore w)
;       ...))
;
;PROS: 
;-No nested structure , so reduced complexity and decomposition is simple
;-Accessibility : no need of helper functions to decompose nested structures
;CONS:
;-Very confusing as we have to maintain eight components within structure.
;- Some features like totalscore and paused? is independent of Ball . 
;its irrelevant to keep world related states and ball related states under
;same structure
;-------------------------------------------------------------------------
;
;2. Alternate data definition
;
;
;A Ball is a 
;make-ball Coordinate Coordinate Time X-dir Y-dir )
;NTERP : 
;, y are X-coordinate and Y-Coordinate of ball
;ime is the total time elapsed after initial world , in ms)
;(define-struct ball ( x y time xdir ydir))
;
;
;TEMPLATE:
;ball-fn : Ball -> ???
;
;(define (ball-fn b)
;  (... (ball-x b) ...(ball-y b) ... (ball-time b)...
;       (x-dir-fn (ball-xdir b))...
;       (y-dir-fn (ball-ydir b))...
;        ))
;
;
;A World is a (make-world Ball posNumber Boolean)
;INTERP :
;total score is the count of total number of correct click on balls
;paused? will tell if the state of worlds is paused or not 
;(define-struct world ( ball totalscore paused?))
;
;TEMPLATE:
;world-fn : World -> ???
;(define (world-fn w)
;  (... (world-ball w) ... (world-totalscore w) ... (world-paused? w) ...))
;  
;  
;Pros:
;-if we are maintaining the time from start no need to take extra counter
;for explosion time . we can simple use t, t+10 to 
;determine explosion time duration
;- Nested structure has separated world relate states from ball 
;related states hence its not confusing 
;- readability and maintenance is a plus point
;
;Cons:
;- taking time will increase arithmetic complexity and operation. 
;- need to calculate velocity at every point .
;- when a world is in pause state, t still need 
;to change because its the time world started.
; -------------------------------------------------------------------------
; 
