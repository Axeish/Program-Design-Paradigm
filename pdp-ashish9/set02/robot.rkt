;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 20) 

(check-location "02" "robot.rkt")

(provide initial-robot)
(provide robot-left)
(provide robot-right)
(provide robot-x)
(provide robot-y)
(provide robot-forward)

;Robot Constants
(define ROBOT-RADIUS 15) ;Integer
(define UP "up") ;String
(define DOWN "down");String
(define LEFT "left");String
(define RIGHT "right");String

  
;Room constants
(define CENTER-X 100); posInteger
(define CENTER-Y 200); posInteger
(define HEIGHT (* CENTER-Y 2)) ; posInteger
(define WIDTH  (* CENTER-X 2)) ; posInteger

;ROBOT-LIMIT-... is Y-co-ordinates of robot when
;it touches top, bottom wall from inside the room
(define ROBOT-LIMIT-BOTTOM (- HEIGHT ROBOT-RADIUS))
(define ROBOT-LIMIT-TOP ROBOT-RADIUS) 

;ROBOT-LIMIT-... is X-co-ordinates of robot when
;it touches left, right wall from inside the room
(define ROBOT-LIMIT-RIGHT (- WIDTH ROBOT-RADIUS))
(define ROBOT-LIMIT-LEFT ROBOT-RADIUS)
           


;A direction is one of:
; - "right"
; - "left"
; - "up"
; - "down"

;INTERP: direction tells which direction the robot is facing

;TEMPLATE:
;direction-fn: direction -> ???
;(define (direction-fn d)
;    (cond
;      [(up? d ) ...]
;      [(down? d)...]
;      [(left? d ) ...]
;      [(right? d)...]))

(define (up? c) (string=? c UP))
(define (down? c) (string=? c DOWN))
(define (left? c) (string=? c LEFT))
(define (right? c) (string=? c RIGHT))

;Robot is a (make-robot x-Coordinate y-Coordinate  direction)
;INTERP: x-Coordinate and Y-coordinate is the location of robot 
;in the universe canvas and direction is where robot is facing
(define-struct robot( x y direction))

;TEMPLATE:
;robot-fn : robot -> ???
(define (robot-fn r)
  (...(robot-x r)...(robot-y r)...))

;Example:
(define ROBO (make-robot 12 13 DOWN))
(define ROBO1 (make-robot 91 21 UP))


;-------------------------------------------------------------------------------

; initial-robot : Coordinate Coordinate -> Robot
; Returns a Robot located at (x,y), facing up.
; Where :x, y represnts the corresponding co-ordinates of robot
; Example:
(begin-for-test
  (check-equal? (initial-robot 0 40 ) 
                (make-robot 0 40 UP) 
                "placing a robot at (0,40)facing up "))

;STRATEGY :function composition
(define (initial-robot x y) 
(make-robot x y UP))

(begin-for-test
  (check-equal? (initial-robot 30 40 ) 
                (make-robot 30 40 UP) 
                "placing a robot at (-30,40)facing up ")
  (check-equal? (initial-robot -30 40 ) 
                (make-robot -30 40 UP) 
                "placing a robot at (-30,40)facing up ")
  (check-equal? (initial-robot 30 -40 ) 
                (make-robot 30 -40 UP) 
                "placing a robot at (30,-40)facing up ")
  (check-equal? (initial-robot -30 -40 ) 
                (make-robot -30 -40 UP) 
                "placing a robot at (-30,-40)facing up "))


; robot-left : Robot -> Robot
; robot-right : Robot -> Robot
; Returns a Robot like r, but turned either 90 degrees left or right.
;Example
(begin-for-test
  (check-equal? (robot-left (make-robot 20 40 UP))
                (make-robot 20 40 LEFT) 
                "turns robot from up to left"))

(begin-for-test
  (check-equal? (robot-right (make-robot 20 40 UP))
                (make-robot 20 40 RIGHT) 
                "turns robot from up to right"))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-left r)
  (cond
    [(up? (robot-direction r))    (make-robot (robot-x r) (robot-y r) LEFT)]
    [(left? (robot-direction r))  (make-robot (robot-x r) (robot-y r) DOWN)]
    [(down? (robot-direction r))  (make-robot (robot-x r) (robot-y r) RIGHT)]
    [(right? (robot-direction r)) (make-robot (robot-x r) (robot-y r) UP)]))

(begin-for-test
  (check-equal? (robot-left (make-robot 30 40 UP))
                (make-robot 30 40 LEFT) 
                "turns robot from up to left")
  (check-equal? (robot-left (make-robot 30 40 LEFT))
                (make-robot 30 40 DOWN) 
                "turns robot from left to down")
  (check-equal? (robot-left (make-robot 30 40 RIGHT)) 
                (make-robot 30 40 UP)
                "turns robot from right to up ")
  (check-equal? (robot-left (make-robot 30 40 DOWN))
                (make-robot 30 40 RIGHT)
                "turns robot from down to right "))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-right r) 
  (cond
    [(up? (robot-direction r)) (make-robot (robot-x r) (robot-y r) RIGHT)]
    [(right? (robot-direction r)) (make-robot (robot-x r) (robot-y r) DOWN)]
    [(down? (robot-direction r)) (make-robot (robot-x r) (robot-y r) LEFT)]
    [(left? (robot-direction r)) (make-robot (robot-x r) (robot-y r) UP)]))
 
(begin-for-test
  (check-equal? (robot-right (make-robot 30 40 UP))
                (make-robot 30 40 RIGHT) 
                "turns robot from up to right")
  (check-equal? (robot-right (make-robot 30 40 LEFT))
                (make-robot 30 40 UP) 
                "turns robot from left to up")
  (check-equal? (robot-right (make-robot 30 40 RIGHT)) 
                (make-robot 30 40 DOWN)
                "turns robot from right to down ")
  (check-equal? (robot-right (make-robot 30 40 DOWN))
                (make-robot 30 40 LEFT)
                "turns robot from down to left "))

 
; robot-forward : Robot NonNegReal -> Robot
; Returns a Robot like r, but moved forward by d pixels.  
; If the robot is inside the room and moving would put any part of the
; robot outside the room, the robot should stop at the wall that it's facing.
;WHERE: 
;d is a distance that is a positive integer 
;Example:
(begin-for-test 
(check-equal? (robot-forward (make-robot 100 -10 LEFT) 500)
               (make-robot -400 -10 LEFT)
               "partial inside top boundary"))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-forward r d)
  (cond
    [(up? (robot-direction r)) (robot-upmove r d)]
    
    [(down? (robot-direction r)) (robot-downmove r d)]
    
    [(left? (robot-direction r)) (robot-leftmove r d)]
    
    [(right? (robot-direction r)) (robot-rightmove r d)])
 )

(begin-for-test
  ;top boundary check
  (check-equal? (robot-forward (make-robot 100 -100 UP) 20)
                (make-robot 100 -120 UP)
                "robot is outside room and goes away from room towards up")
  (check-equal? (robot-forward (make-robot 100 300 UP) 20)
                (make-robot 100 280 UP)
                "robot is inside room and goes up , not touching the wall")
  (check-equal? (robot-forward (make-robot 100 300 UP) 400)
                (make-robot 100 15 UP)
                "robot is inside room ,tries to go out but get flushed at wall")
  (check-equal? (robot-forward (make-robot 100 500 UP) 20)
                (make-robot 100 480 UP)
                "robot is  below room comes towards room but do not reach room")
  (check-equal? (robot-forward (make-robot 100 500 UP) 200)
                (make-robot 100 300 UP)
                "robot is  below room,comes towards room and get inside ")
  (check-equal? (robot-forward (make-robot 100 500 UP) 600)
                (make-robot 100 15 UP)
                "robot is below room, get inside , but cannot 
               leave from the opposite wall of room")
  (check-equal? (robot-forward (make-robot 100 -10 UP) 50)
                (make-robot 100 -60 UP)
                "robot is partially inside room , on top wall, 
                center inside room , tries to go out")
  (check-equal? (robot-forward (make-robot 100 15 UP) 600)
                (make-robot 100 15 UP)
                "robot is flushed on top wall , 
                tries but cannot leave in up direction")
  (check-equal? (robot-forward (make-robot -10 300 UP) 500)
                (make-robot -10 -200 UP)
                "partial inside left boundary")
  (check-equal? (robot-forward (make-robot 210 300 UP) 500)
                (make-robot 210 -200 UP)
                "partial inside right boundary")
  ;down boundry check
  (check-equal? (robot-forward (make-robot 100 500 DOWN) 20)
                (make-robot 100 520 DOWN)
                "robot is outside room and goes away from room towards DOWN")
  (check-equal? (robot-forward (make-robot 100 300 DOWN) 20)
                (make-robot 100 320 DOWN)
                "robot is inside room and goes DOWN , not touching the wall")
  (check-equal? (robot-forward (make-robot 100 300 DOWN) 400)
                (make-robot 100 385 DOWN)
                "robot is inside room ,tries to go out but get flushed at wall")
  (check-equal? (robot-forward (make-robot 100 -100 DOWN) 20)
                (make-robot 100 -80 DOWN)
                "robot is  above room comes towards room but do not reach room")
  (check-equal? (robot-forward (make-robot 100 -100 DOWN) 200)
                (make-robot 100 100 DOWN)
                "robot is  above room,comes towards room and get inside ")
  (check-equal? (robot-forward (make-robot 100 -100 DOWN) 600)
                (make-robot 100 385 DOWN)
                "robot is above room, get inside , but cannot 
               leave from the opposite wall of room")
  (check-equal? (robot-forward (make-robot 100 400 DOWN) 50)
                (make-robot 100 450 DOWN)
                "robot is partially inside room , on bottom wall, 
                center inside room , tries to go out")
  (check-equal? (robot-forward (make-robot 100 385 DOWN) 600)
                (make-robot 100 385 DOWN)
                "robot is flushed on top wall , 
                tries but cannot leave in DOWN direction")
  (check-equal? (robot-forward (make-robot -10 300 DOWN) 500)
                (make-robot -10 800 DOWN)
                "partial inside left boundary")
  (check-equal? (robot-forward (make-robot 210 300 DOWN) 500)
                (make-robot 210 800 DOWN)
                "partial inside right boundary")    
  ;Left boundarycheck 
  (check-equal? (robot-forward (make-robot -100 200 LEFT) 20)
                (make-robot -120 200 LEFT)
                "robot is outside room and goes away from room towards LEFT")
  (check-equal? (robot-forward (make-robot 100 200 LEFT) 30)
                (make-robot 70 200 LEFT)
                "robot is inside room and goes LEFT , not touching the wall")
  (check-equal? (robot-forward (make-robot 100 200 LEFT) 400)
                (make-robot 15 200 LEFT)
                "robot is inside room ,tries to go out but get flushed at wall")
  (check-equal? (robot-forward (make-robot 500 200 LEFT) 20)
                (make-robot 480 200 LEFT)
                "robot is  right of room comes towards room but do not reach room")
  (check-equal? (robot-forward (make-robot 300 200 LEFT) 200)
                (make-robot 100 200 LEFT)
                "robot is  right of room,comes towards room and get inside ")
  (check-equal? (robot-forward (make-robot 300 200 LEFT) 600)
                (make-robot 15 200 LEFT)
                "robot is right of room, get inside , but cannot 
               leave from the opposite wall of room")
  (check-equal? (robot-forward (make-robot -10 200 LEFT) 50)
                (make-robot -60 200 LEFT)
                "robot is partially inside room , on LEFT wall, 
                center inside room , tries to go out")
  (check-equal? (robot-forward (make-robot 15 100 LEFT) 600)
                (make-robot 15 100 LEFT)
                "robot is flushed on left wall , 
                tries but cannot leave in LEFT direction")
  (check-equal? (robot-forward (make-robot 100 -10 LEFT) 500)
                (make-robot -400 -10 LEFT)
                "partial inside top boundary")
  (check-equal? (robot-forward (make-robot 100 395 LEFT) 500)
                (make-robot -400 395 LEFT)
                "partial inside below boundary")
  ;right boundary check 
  (check-equal? (robot-forward (make-robot 300 200 RIGHT) 20)
                (make-robot 320 200 RIGHT)
                "robot is outside room and goes away from room towards RIGHT")
  (check-equal? (robot-forward (make-robot 100 200 RIGHT) 30)
                (make-robot 130 200 RIGHT)
                "robot is inside room and goes RIGHT , not touching the wall")
  (check-equal? (robot-forward (make-robot 100 200 RIGHT) 400)
                (make-robot 185 200 RIGHT)
                "robot is inside room ,tries to go out but get flushed at wall")
  (check-equal? (robot-forward (make-robot -100 200 RIGHT) 20)
                (make-robot -80 200 RIGHT)
                "robot is  left of room comes towards room but do not reach room")
  (check-equal? (robot-forward (make-robot -100 200 RIGHT) 200)
                (make-robot 100 200 RIGHT)
                "robot is  left of room,comes towards room and get inside ")
  (check-equal? (robot-forward (make-robot -100 200 RIGHT) 600)
                (make-robot 185 200 RIGHT)
                "robot is left of room, get inside , but cannot 
               leave from the opposite wall of room")
  (check-equal? (robot-forward (make-robot 190 200 RIGHT) 50)
                (make-robot 240 200 RIGHT)
                "robot is partially inside room , on RIGHT wall, 
                center inside room , tries to go out")
  (check-equal? (robot-forward (make-robot 185 100 RIGHT) 600)
                (make-robot 185 100 RIGHT)
                "robot is flushed on right wall , 
                tries but cannot leave in RIGHT direction")
  (check-equal? (robot-forward (make-robot 100 -10 RIGHT) 500)
                (make-robot 600 -10 RIGHT)
                "partial inside top boundary")
  (check-equal? (robot-forward (make-robot 100 395 RIGHT) 500)
                (make-robot 600 395 RIGHT)
                "partial inside below boundary"))


;robot-upmove : Robot NonNegReal -> Robot
;Will move robot in up direction by d pixels
; If the robot is inside the room and any part touches the top wall,
;the robot should stop at the TOP wall
;WHERE: 
;Robot r is facing towards UP

;Example:
(begin-for-test
  (check-equal?  (robot-upmove (make-robot 30 40 UP ) 20)
                (make-robot 30 20 UP) 
                "moving robot in up direction "))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-upmove r d)
  (cond
    [(robot-up-noboundary? r) 
     (make-robot (robot-x r) (- (robot-y r) d)(robot-direction r))]
    
    [(>= (robot-y r) ROBOT-LIMIT-TOP) 
     (make-robot (robot-x r) 
                 (robot-upmove-toboundary r d) 
                 (robot-direction r))]))
;all possible test case covered in robot-forward function


;robot-leftmove : Robot NonNegReal -> Robot
;Will move robot in left direction by d pixels
; If the robot is inside the room and any part touches the Left wall,
;the robot should stop at the left wall
;WHERE: 
;Robot r is facing towards left
;Example:
(begin-for-test
  (check-equal?  (robot-leftmove (make-robot 50 40 LEFT ) 20)
                (make-robot 30 40 LEFT) 
                "moving robot in left direction "))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-leftmove r d)
  (cond 
    [(robot-left-noboundary? r) 
     (make-robot (- (robot-x r) d) (robot-y r)(robot-direction r))]
    
    [(>= (robot-x r) ROBOT-LIMIT-LEFT)   
     (make-robot (robot-leftmove-toboundary r d) 
                 (robot-y r)
                 (robot-direction r))]))
;all possible test case covered in robot-forward function

;robot-downmove : Robot NonNegReal -> Robot
;Will move robot in down direction by d pixels
; If the robot is inside the room and any part touches the bottom wall,
;the robot should stop at the bottom wall
;WHERE: 
;Robot r is facing towards down
;Example:
(begin-for-test
  (check-equal?  (robot-downmove (make-robot 50 40 DOWN ) 20)
                (make-robot 50 60 DOWN) 
                "moving robot in down direction "))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-downmove r d)
  (cond
    [(robot-down-noboundary? r) 
     (make-robot (robot-x r) (+ (robot-y r) d)(robot-direction r))]
    
    [(<= (robot-y r) ROBOT-LIMIT-BOTTOM) 
     (make-robot (robot-x r) 
                 (robot-downmove-toboundary r d) 
                 (robot-direction r))]))
;all possible test case covered in robot-forward function

;robot-rightmove : Robot NonNegReal -> Robot
;Will move robot in dright direction by d pixels
; If the robot is inside the room and any part touches the right wall,
;the robot should stop at the right wall
;WHERE: 
;Robot r is facing towards down
(begin-for-test
  (check-equal?  (robot-rightmove (make-robot 50 40 RIGHT ) 20)
                (make-robot 70 40 RIGHT) 
                "moving robot in right direction "))

;STRATEGY: Data Decomposition of r :Robot
(define (robot-rightmove r d)
  (cond
    [(robot-right-noboundary? r) 
     (make-robot (+ (robot-x r) d)(robot-y r)(robot-direction r))]
    [(<= (robot-x r) ROBOT-LIMIT-RIGHT) 
     (make-robot (robot-rightmove-toboundary r d)
                 (robot-y r)
                 (robot-direction r))]))
;all possible test case covered in robot-forward function

;robot-up-noboundary? : Robot  -> Boolean
; will move robot in up direction by pixel d
; WHERE 
; robot is facing up 
;Example:
(begin-for-test
  (check-equal?  (robot-up-noboundary? (make-robot -30 -40 UP ) )
                #t
                "checking robot in up direction "))
;STRATEGY: Data Decomposition on r : Robot
(define (robot-up-noboundary? r)
  (or (< (robot-x r) ROBOT-LIMIT-LEFT) 
      (> (robot-x r) ROBOT-LIMIT-RIGHT) 
      (< (robot-y r) ROBOT-LIMIT-TOP)))
;all possible test case covered in robot-forward function

;robot-down-noboundary? :Robot  -> Boolean
; will move robot in down direction by pixel d
; WHERE 
; robot is facing down 
;Example:
(begin-for-test
  (check-equal?  (robot-down-noboundary? (make-robot 600 400 DOWN ) )
                #t
                "checking robot in down direction "))
;STRATEGY: Data Decomposition on r : Robot
(define (robot-down-noboundary? r)
  (or (< (robot-x r) ROBOT-LIMIT-LEFT) 
      (> (robot-x r) ROBOT-LIMIT-RIGHT) 
      (> (robot-y r) ROBOT-LIMIT-BOTTOM)))
;all possible test case covered in robot-forward function

;robot-left-noboundary? :Robot  -> Boolean
; will move robot in left direction by pixel d
; WHERE 
; robot is facing left 
;Example:
(begin-for-test
  (check-equal?  (robot-left-noboundary? (make-robot 600 400 LEFT ) )
                #t
                "checking robot in left direction "))
;STRATEGY: Data Decomposition on r : Robot
(define (robot-left-noboundary? r)
  (or (< (robot-y r) ROBOT-LIMIT-TOP) 
      (> (robot-y r) ROBOT-LIMIT-BOTTOM) 
      (< (robot-x r) ROBOT-LIMIT-LEFT)))
;all possible test case covered in robot-forward function

;robot-right-noboundary? : Robot  -> Boolean
; will move robot in right direction by pixel d
; WHERE 
; robot is facing right 
;Example:
(begin-for-test
  (check-equal?  (robot-right-noboundary? (make-robot 600 400 RIGHT ) )
                #t
                "checking robot in right direction "))
;STRATEGY: Data Decomposition on r : Robot
(define (robot-right-noboundary? r)
  (or (< (robot-y r) ROBOT-LIMIT-TOP)
      (> (robot-y r) ROBOT-LIMIT-BOTTOM)
      (> (robot-x r) ROBOT-LIMIT-RIGHT)))
;all possible test case covered in robot-forward function

;robot-upmove-toboundary :ROBOT NonReal -> Integer
;will return y coordinates of robot after moving d pixel
;WHERE:
; robot is facing up 
;Example:
(begin-for-test 
  (check-equal? (robot-upmove-toboundary (make-robot 100 100 UP) 300)
  15
  "top-boundary of the room"))

;STRATEGY: Data Decomposition on r : Robot
(define (robot-upmove-toboundary r d)
  (if (< (- (robot-y r) d) ROBOT-LIMIT-TOP ) 
      ROBOT-LIMIT-TOP 
      (- (robot-y r) d)))
;all possible test case covered in robot-forward function

;robot-downmove-toboundary :ROBOT NonReal -> Integer
;will return y coordinates of robot after moving d pixel
;WHERE:
; robot is facing down 
;Example:
(begin-for-test 
  (check-equal? (robot-downmove-toboundary (make-robot 100 100 DOWN) 300)
  385
  "robot-rightmove-toboundary of the room"))

;STRATEGY: Data Decomposition on r : Robot
(define (robot-downmove-toboundary r d)
 (if (> (+ (robot-y r) d) ROBOT-LIMIT-BOTTOM ) 
     ROBOT-LIMIT-BOTTOM 
     (+ (robot-y r) d)) )
;all possible test case covered in robot-forward function

;robot-leftmove-toboundary :ROBOT NonReal -> Integer
;will return x coordinates of robot after moving d pixel
;WHERE:
; robot is facing left 
;Example:
(begin-for-test 
  (check-equal? (robot-leftmove-toboundary (make-robot 100 100 LEFT) 300)
  15
  "robot-leftmove-toboundary of the room"))

;STRATEGY: Data Decomposition on r : Robot
(define (robot-leftmove-toboundary r d)
  (if (< (- (robot-x r) d) ROBOT-LIMIT-LEFT ) 
      ROBOT-LIMIT-LEFT 
      (- (robot-x r) d)))
;all possible test case covered in robot-forward function

;robot-rightmove-toboundary :ROBOT NonReal -> Integer
;will return y coordinates of robot after moving d pixel
;WHERE:
; robot is facing right 
;Example:
(begin-for-test 
  (check-equal? (robot-rightmove-toboundary (make-robot 100 100 RIGHT) 300)
  185
  "robot-rightmove-toboundary of the room"))

;STRATEGY: Data Decomposition on r : Robot
(define (robot-rightmove-toboundary r d)
  (if (> (+ (robot-x r) d) ROBOT-LIMIT-RIGHT ) 
      ROBOT-LIMIT-RIGHT 
      (+ (robot-x r) d)))

;all possible test case covered in robot-forward function

;------------------------------------------------------------------------------------------------------