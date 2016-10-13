;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define  TIME-ON-TASK 65)

(check-location "04" "worm.rkt")

(provide INITIAL-WORLD)
(provide next-world)
(provide key-handler)
(provide end?)
(provide world-worm)
(provide create-worm)
(provide worm-length)
(provide worm-head-x)
(provide worm-head-y)
(provide replace-food)
(provide replace-worm)
(provide posns-overlap?)


;canvas constants
(define CENTER-X 150)                  ;pixels
(define CENTER-Y 150)                  ;pixels
(define WIDTH (* 2 CENTER-X))          ;pixels
(define HEIGHT (* 2 CENTER-Y))         ;pixels
(define MTS (empty-scene WIDTH HEIGHT)); image
(define TEXTCOLOR "black")
(define TEXTSIZE 15)

(define SEGEMENT-DIAMETER 10)
(define FOOD-RADIUS 5)
(define LEFT-BOUNDARY FOOD-RADIUS)
(define RIGHT-BOUNDARY (- WIDTH FOOD-RADIUS))
(define TOP-BOUNDARY FOOD-RADIUS)
(define BOTTOM-BOUNDARY ( - HEIGHT FOOD-RADIUS))


;image constants
(define WORM-SEG-IMG (circle FOOD-RADIUS "solid" "red"))
(define FOOD-IMG (circle FOOD-RADIUS "solid" "green"))
(define HIT-WALL-IMG (text "WORM HIT BORDER    " TEXTSIZE TEXTCOLOR))
(define HIT-SELF-IMG (text "WORM BITE SELF     " TEXTSIZE TEXTCOLOR))


(define TEST-HIT-WALL-IMG (text "WORM HIT BORDER    0" TEXTSIZE TEXTCOLOR))
(define tick-rate 0.5)


;DATA DEFINITIONS
;-----------------------------------------------------------------------


;Dir is one of the:
; - "right"
; - "left"
; - "up"
; - "down"
; INTERP :  represents a  direction of snake.
(define RIGHT "right")
(define LEFT "left")
(define UP "up")
(define DOWN "down")

;<Dir predicates> :Dir -> Boolean
;Returns true if d is the Direction indicated by the function name.
;STrategy:Data Decomposition on dir: Dir
(define (right? d) (string=? d RIGHT))
(define (left? d)  (string=? d LEFT))
(define (up? d) (string=? d UP))
(define (down? d)  (string=? d DOWN))

; TEMPLATE
; dir-fn: Dir -> ???
;(define (dir-fn dir)
;  (cond
;    [(right? dir) ...]
;    [(left? dir) ...]
;    [(up? dir) ...]
;    [(down? dir) ...]))



;LoPosns is a List of Posns
;A LoPosns is one of:
; - empty 
; - (cons Posn LoPosns)

;INTERP: empty is a list with no elements 
;(cons Posn LoPosns) represents a list of posns 
; where posn is the first element and  LoPosns is the rest of the elements

;Example :
(define LOP (list (make-posn 10 20 )
                  (make-posn 10 25 )
                  (make-posn 10 30 )))

;TEMPLATE:
;(define (loposns-fn l)
;  (cond 
;    [(empty? l)  '()]
;    [(cons? l)
;     (... (first l) ...  
;          (loposns-fn (rest l )) ... ) ]))
;


; A KeyEvent is one of:
; - "a"
; - "b"
; - "c"
; - ...
; - "right"
; - "left"
; - "up"
; - "down"

; INTERP: A KeyEvent is a key-press in a Universe program.

; TEMPLATE:
; keyevent-fn : KeyEvent -> ???
;(define (keyevent-fn kev)
;  (cond
;    [(key=? kev "a") ...]
;    [(key=? kev "b") ...]
;    [(key=? kev "c") ...]
;    ...))


;A worm is a (make-worm  Dir LoPosns )
;INTERP: Dir is direction where the worm is moving 
;LoPosns is position of center of each of segments of the worm
(define-struct worm (dir LoPosns))

;Example
(define WORM1 (make-worm "right" (list (make-posn 65 45 )
                             (make-posn 75 45 ) 
                             (make-posn 85 45 ))))
(define WORM2 (make-worm "down" (list (make-posn 25 35 )
                             (make-posn 35 35 ) 
                             (make-posn 45 35 ))))

;TEMPLATE:
;worm-fn : WORM -> ???
(define (worm-fn w)
  (... (dir-fn (worm-dir w)) ... 
       (loposns-fn (worm-LoPosns w)) ... ))


;A world is a (make-world Worm Posn)
;INTERP: food is a new random posn of the center of food  
(define-struct world (worm food))

;Example
(define WORLD1 (make-world WORM1 (make-posn 45 45)))
(define INITIAL-WORLD (make-world (make-worm 
                                   "right"
                                   (list (make-posn 95 15)))
                                   (make-posn 25 85)))

;TEMPLATE:
;world-fn : WORLD -> ???
(define (world-fn w)
  (...  
   (world-worm w) ...  
   (world-food w) ... ))




;RUN SIMULATION
;--------------------------------------------------------------

;run : World -> World 
; will start stimulation 
; STRATEGY: function composition
(define (run w)
  ( big-bang w
            (on-tick next-world tick-rate)
            (on-key key-handler)
            (to-draw render )
            (stop-when end? render-last )
            ))


;next-world : World -> World
; will return new world 
;Example 
(begin-for-test
  (check-equal? (next-world INITIAL-WORLD)
                (make-world (make-worm "right" 
                                       (list 
                                        (make-posn 105 15))) 
                            (make-posn 25 85))
                "next-world"))
                        

;STRATEGY: Data Decomposition of w : World
                
(define (next-world w)
  (if (eat-food? (next-worm (world-worm w)) (world-food w))
      (grow-world (world-worm w))
     
      (make-world (move-worm (world-worm w)) (world-food w))))

;Testing
(begin-for-test
  (check-random (next-world 
                 (make-world
                  (make-worm "right" (list (make-posn 105 15)))
                  (make-posn 115 15)))
                (make-world
                 (make-worm
                  "right"
                  (list (make-posn 115 15) (make-posn 105 15)))
                 (random-posn  WIDTH HEIGHT SEGEMENT-DIAMETER FOOD-RADIUS))
                ))

; key-handler  : World KeyEvent -> World
; Call handler according to the key pressed
;Example:
(begin-for-test
 (check-equal?  (key-handler INITIAL-WORLD DOWN)
                (make-world 
                 (make-worm 
                  "down" 
                  (list (make-posn 95 15)))
                 (make-posn 25 85))
                "on key down"))
;STRATEGY :Data Decomposition of w: World
(define (key-handler w k)
  (make-world ( key-handle-worm (world-worm w) k)
              (world-food w)))

;render: World -> Image
;will return Image of new World
; Example 
(begin-for-test
  (check-equal?
   (render INITIAL-WORLD)
   (place-image FOOD-IMG 25 85
  (render-worm (world-worm INITIAL-WORLD)
               ))
   "new worlld"))
;STRATEGY: Data Decomposition of w : World
(define (render w)
  (place-image FOOD-IMG (get-x (world-food w))(get-y (world-food w))
  (render-worm (world-worm w)
               )))

;render-last: World -> Image
; render the game over scene
;Example
(begin-for-test
  (check-equal? (render-last INITIAL-WORLD)
                (place-image TEST-HIT-WALL-IMG 130 230
                    (place-image FOOD-IMG 25 85
  (render-worm (world-worm INITIAL-WORLD)
               )))
                "Game-Over"))
;STRATEGY : Data Decomposition on w :World
(define (render-last w)
  (place-image (display-message (world-worm w)) 130 230 (render w)))


; FUNCTION IMPLEMENTATION
;------------------------------------------------------


;CLOCK TICK HANDLER
;----------------------

;grow-world : Worm -> World
; will retunr a new world with worm that is increased in length and new food
;STRATEGY: Function Composition

(define (grow-world w)
  (make-world (grow-worm w)  (replacer-food  w)))
; Cannot be tested


;next-worm : Worm -> Posn
;will return new position of center of snake head
;example
(begin-for-test
  (check-equal? (next-worm WORM1)
                (make-posn 75 45)
                "position of head"))
;STRATEGY: Data Decomposition of  s: Worm
(define (next-worm s)
  (next-head (get-dir s) (get-segment s)))


  
;next-head : Dir LoPosns -> Posn 
; will retune new posn for the center of head of snake 
; Example
(begin-for-test
  (check-equal? (next-head UP (list (make-posn 75 35)))
                (make-posn 75 25)
                "position of head"))  
;STRATEGY: Data Decomposition of dir :Dir
(define (next-head dir list)
  (cond [(up?    dir) (posn-move-first list  0 -10)]
        [(down?  dir) (posn-move-first list  0  10)]
        [(left?  dir) (posn-move-first list -10  0)]
        [(right? dir) (posn-move-first list  10  0)]))

;Testing
(begin-for-test
  (check-equal? (next-head UP (list (make-posn 75 35)))
                (make-posn 75 25)
                "position of head")
  (check-equal? (next-head DOWN (list (make-posn 75 35)))
                (make-posn 75 45)
                "position of head")
  (check-equal? (next-head LEFT (list (make-posn 75 35)))
                (make-posn 65 35)
                "position of head")
  (check-equal? (next-head RIGHT (list (make-posn 75 35)))
                (make-posn 85 35)
                "position of head"))

;posn-move-first : LoPosns Coordinate Coordinate -> Posns
; will return new posn of center of head of worm
;example
(begin-for-test
  (check-equal? (posn-move-first (worm-LoPosns WORM1) 10 0)
                (make-posn 75 45)
                "new position"))
;STRATEGY: Data Decomposition of l :LoPosns
(define (posn-move-first l dx dy)
  (posn-move (first l) dx dy))
                                
;posn-move: Posn coordinate Coordinate-> Posn
;will return new position of center of each segment
;example:
(begin-for-test
  (check-equal?  (posn-move (make-posn 12 13 ) 10 20)
                 (make-posn 22 33)
                 "new position"))
;STRATEGY : Function Composition
(define (posn-move p dx dy)
   (make-posn (+ (get-x p) dx) 
              (+ (get-y p) dy )))


;grow-worm : Worm -> Worm
; will increase the size of the worm 
;example
(begin-for-test
  (check-equal?  (grow-worm WORM1)
                 (make-worm
                  "right"
                  (list 
                   (make-posn 75 45) 
                   (make-posn 65 45) 
                   (make-posn 75 45) 
                   (make-posn 85 45)))
                 "grow-worm"))
  ;STRATEGY: Data Decomposition on s: Worm
(define (grow-worm s )
  (make-worm (get-dir s ) 
             (cons  (next-worm s) (get-segment s))))


;move-worm : Worm -> Worm
; will move the worm towards the direction
;example
(begin-for-test
  (check-equal?  (move-worm WORM1)
                 (make-worm
                  "right"
                  (list 
                   (make-posn 75 45) 
                   (make-posn 65 45) 
                   (make-posn 75 45)))
                 "move-worm"))
  ;STRATEGY: Function Composition  
(define (move-worm s)
  (make-worm (get-dir s ) 
             (cons (next-worm s) 
                   (del-last (get-segment s)))))

;replacer-food : Worm -> Posn
;will return new random position for center of a food              
;STRATEGY: Data Decomposition on worm : Worm
(define (replacer-food worm)
  (random-food (worm-LoPosns worm)))
;Cannot test random value

; random-posn : Pixel  Pixel Pixel Pixel -> Posn
; Returns a random posn within a width x height canvas.
; WHERE: the returned posn satisfies ???
(define (random-posn width height interval offset)
  (make-posn
   (+ offset (* interval (random (quotient width interval))))
   (+ offset (* interval (random (quotient height interval))))))

; random-food : ListOfPosn -> Food
; Returns a valid Coordinates of a food
;Example:
(begin-for-test
  (check-random (random-food '())
                (random-posn  WIDTH HEIGHT SEGEMENT-DIAMETER FOOD-RADIUS)))
;STRATEGY: Function Composition
(define (random-food not-allowed)
  (food-check
   (random-posn WIDTH HEIGHT SEGEMENT-DIAMETER FOOD-RADIUS)
   not-allowed))

; food-check : Food ListOfPosn -> Posn
; will return valid list of positon of Food
;Example:
(begin-for-test
  (check-random (food-check (make-posn 12 13) (list (make-posn 12 13)))
                (random-posn  WIDTH HEIGHT SEGEMENT-DIAMETER FOOD-RADIUS)))
; Strategy: generative recursion
(define (food-check candidate-food not-allowed)
  (if (posns-overlap? candidate-food not-allowed)
      (random-food not-allowed)
      candidate-food))


;eat-food? Posn Posn -> Boolean
;check weather both the posn are overlapping or not
;Example
(begin-for-test
  (check-true (eat-food? 
               (make-posn 12 12)
               (make-posn 12 12))))

;STRATEGY: Function Composition
(define (eat-food? w s )
 ( equal?  w s))


;del-last : LoPosns -> LoPosns
; will return a new LoPosns after deleting the last element
; Example
(begin-for-test
  (check-equal?
   (del-last (list
             (make-posn 10 15)
             (make-posn 15 15)
             (make-posn 20 15)))
   (list
    (make-posn 10 15)
    (make-posn 15 15))
   "delete last of a list"))
;Strategy: Data Decomposition on LoPosns: LoPosn
(define (del-last LoPosns)
  (cond [(empty? (rest LoPosns)) empty]
        [else (cons (first LoPosns) 
                    (del-last (rest LoPosns)))]))


        
; replace-food : World Posn -> World
; Inserts a piece of food into the world at the given Coordinates,
; replacing the existing food.
; WHERE: The food does not overlap with any of the worm's segments.
;Example
(begin-for-test
  (check-equal? (replace-food 
                 INITIAL-WORLD
                 (make-posn 25 65))
                (make-world
                 (make-worm "right" (list (make-posn 95 15)))
                 (make-posn 25 65))
                "new posn for food"))

;STRATEGY : Data Decomposition of world : World
(define (replace-food world p)
  (make-world  (world-worm world) p))

;DRAW-HANDLER
;----------------------------------

  
;render-worm : Worm -> Image
;will return Image of worm
;Example 
(begin-for-test
  (check-equal? (render-worm WORM1)
                (place-image WORM-SEG-IMG 65 45 
                    (place-image WORM-SEG-IMG 75 45 
                        (place-image WORM-SEG-IMG 85 45 MTS)))
                "worm Image"))
;STRATEGY: Data Decomposition on w: Worm
(define (render-worm w)
  (render-worm-seg (worm-LoPosns w)))
 

;render-worm-seg : LoPosns -> Image
;will return Image of worm
;Example 
(begin-for-test
  (check-equal? (render-worm-seg (worm-LoPosns WORM1))
                (place-image WORM-SEG-IMG 65 45 
                    (place-image WORM-SEG-IMG 75 45 
                        (place-image WORM-SEG-IMG 85 45 MTS)))
                "worm Image"))
;STRATEGY: Data Decomposition on l: LoPosns
(define (render-worm-seg l)
  (cond
    [(empty? l ) MTS ]
    [else (render-each-seg (first l ) (render-worm-seg (rest l)))]))

;render-each-seg : Posn ->Image
;will return image of a segment whose center is at posn
;example
(begin-for-test
  (check-equal? (render-each-seg (make-posn 10 12) MTS)
                (place-image  WORM-SEG-IMG 10 12 MTS)
                "each segment-img "))
;STRATEGY: Function-composition
(define (render-each-seg p img)
  (place-image 
   WORM-SEG-IMG
   (get-x p )
   (get-y p)
   img))
                
                
                

;KEY HANDLER
;----------------------------------------


;key-handle-worm :Worm KeyEvent -> Worm
;Call handler according to key pressed
(begin-for-test
 (check-equal? 
  (key-handle-worm 
   (make-worm
    "right" 
    (list
     (make-posn 95 15))) DOWN)
  
   (make-worm "down" (list (make-posn 95 15)))
   "new worm "))
;STRATEGY :Data Decomposition on w : Worm
(define (key-handle-worm worm k)
  (make-worm 
   (worm-change-dir? (worm-dir worm) k)
   (worm-LoPosns worm)))

;worm-change-dir : Dir KeyEvent -> Dir
;will return new direction based on Keyevent
;example
(begin-for-test 
  (check-equal? (worm-change-dir? UP RIGHT)
                RIGHT
                "right move"))
; STRATEGY: Data Decomposition on k :KeyEvent 
(define (worm-change-dir? dir key)
  (cond
    [(key=? key "up") UP]
    [(key=? key "down") DOWN]
    [(key=? key "left") LEFT]
    [(key=? key "right") RIGHT]
    [else dir]))
;example
(begin-for-test 
  (check-equal? (worm-change-dir? UP "shift")
                UP
                "no change")
  (check-equal? (worm-change-dir? UP LEFT)
                LEFT
                "Left move")
  (check-equal? (worm-change-dir? DOWN UP)
                UP
                "up move")
  (check-equal? (worm-change-dir? LEFT DOWN)
                DOWN
                "down move"))


;end? : World -> Boolean
;check for game over
;example
(begin-for-test
  (check-equal? (end? (make-world 
                       (make-worm "up" 
                                  (list (make-posn 145 -5))) 
                       (make-posn 25 85)))
               #true 
               "end game"))
;STRATEGY: Data Decomposition on w: World
(define (end? w)
  (hit-worm? (world-worm w)))


;hit-worm? Worm -> Boolean
; will return true if worm has hit wall or bitten
; example 
(begin-for-test
  (check-equal? (hit-worm? (make-worm "up" (list (make-posn 145 -5))))
                #true
                "hit on wall"))
;STRATEGY: Data Decomposition on w : Worm
               
(define (hit-worm? w)
  (hit-head? (worm-LoPosns w)))

;Testing 
(begin-for-test
  (check-equal? (hit-worm? (make-worm
                          "up"
                          (list
                           (make-posn 165 155)
                           (make-posn 165 165)
                           (make-posn 165 155))))
                #true
                "hit self"))

;hit-head? : LoPosns -> Boolean
; will return true if head of sanke hit self or wall
(begin-for-test
  (check-equal? (hit-head? (list (make-posn 145 -5)))
                #true
                "head it wall"))
;STRATEGY: Data Decomposition of l :LoPosns
          
(define (hit-head? l)  
(or (hit-wall? (first l)) (hit-self? l)))

;hit-wall? : Posn -> Boolean
; will return true if Posn is out ofboundary
;example:
(begin-for-test
  (check-equal? (hit-wall? (make-posn -5 10))
                #true
                "hit wall"))
;STRATEGY: data Decomposition on s: Posn
(define (hit-wall? s)
   (or (< (get-x s) LEFT-BOUNDARY ) 
       (> (get-x s) RIGHT-BOUNDARY)
       (< (get-y s) TOP-BOUNDARY ) 
       (> (get-y s) BOTTOM-BOUNDARY)))

;Testing
(begin-for-test
  (check-equal? (hit-wall? (make-posn 500 10))
                #true
                "hit right wall")
  (check-equal? (hit-wall? (make-posn -50 10))
                #true
                "hit left wall")
  (check-equal? (hit-wall? (make-posn 20 510))
                #true
                "hit bottom wall")
  (check-equal? (hit-wall? (make-posn 50 -10))
                #true
                "hit top wall")
  (check-equal? (hit-wall? (make-posn 50 10))
                #false
                "no hit wall"))               
;hit-self?: LoPosns -> Boolean               
; will return true if any of the worm segments overlap with worm head
;example:
(begin-for-test
  (check-equal? (hit-self? (list 
                            (make-posn 10 20) 
                            (make-posn 10 30) 
                            (make-posn 10 20)))
                #true
                "self bite"))
;STRATEGY: Data Decomposition on l : LoPosns
(define (hit-self? l)
    ( posns-overlap? (first l) (rest l)))

; posns-overlap? : Posn ListOfPosn -> Boolean
; Returns true if p overlaps with any elements of ps.
;  Two posns touching at only their outer edges are not overlapping.
;Example:
(begin-for-test
  (check-equal? (posns-overlap? (make-posn 12 13 )
                                (list (make-posn 12 13)
                                      (make-posn 14 13)))
                #true
                "overlap"))
;STRATEGY: Function Composition
(define (posns-overlap? p ps)
   (member p ps))



;display-message: Worm -> Image
; render the game over scene
;Example
(begin-for-test
  (check-equal? (display-message (make-worm
                          "up"
                          (list
                           (make-posn 165 155)
                           (make-posn 165 165)
                           (make-posn 165 155))))
                (beside HIT-SELF-IMG   
          (text "2" TEXTSIZE TEXTCOLOR))
                "image of message"))

;STRATEGY: Function Composition 
(define (display-message worm  )
  (beside (message worm) 
          (text 
           (number->string (- (worm-length worm) 1))
           TEXTSIZE
           TEXTCOLOR)))

;message: Worm -> Image
; render the game over scene
;Example
(begin-for-test
  (check-equal? (message (world-worm INITIAL-WORLD))
                HIT-WALL-IMG
                "Hit Wall"))
;STRATEGY : Data Decomposition on Worm :Worm
(define (message worm)
  (if (hit-self? (worm-LoPosns worm)) 
      HIT-SELF-IMG
      HIT-WALL-IMG))

;Testing
(begin-for-test
  (check-equal? (message (make-worm
                          "up"
                          (list
                           (make-posn 165 155)
                           (make-posn 165 165)
                           (make-posn 165 155))))
                HIT-SELF-IMG
                "bit self"))


; worm-length : Worm -> PosInt
; Returns the number of segments in the given worm.
;STRATEGY: Function Composition
(define (worm-length worm)
  ( length (worm-LoPosns worm)))

; worm-head-x : Worm -> Coordinate
; Returns the x position of the center of the worm's lead segment.
;Example 
(begin-for-test
  (check-equal? (worm-head-x WORM2)
                25
                "x-coordinate of head"))
;STRATEGY : Function Composition 
(define (worm-head-x worm)
  (get-x (worm-head worm)))
; worm-head-y : Worm -> Coordinate
; Returns the  y position of the center of the worm's lead segment.
;Example 
(begin-for-test
  (check-equal? (worm-head-y WORM2)
                35
                "y-coordinate of head"))
;STRATEGY : Function Composition 
(define (worm-head-y worm)
  (get-y ( worm-head worm)))

;worm-head : Worm -> Posn
; will return position of center of  head os snake
;Example
(begin-for-test
  (check-equal? (worm-head WORM2)
                (make-posn 25 35)
                "head of worm "))
;STRATEGY : Data Decomposition of worm: Worm
(define (worm-head worm)
  (head-list (get-segment worm)))


; create-worm : ListOfPosn -> Worm
; Creates a worm from the given Posns, using the first Posn in the list
; as the worm's head, and the rest of the list, in that order, 
; as the worm's body.
; The resulting Worm may have other attributes of any value.
; WHERE: the list of posns are contiguous and form a valid worm
; example
(begin-for-test
  (check-equal? (create-worm (list 
                   (make-posn 25 35)
                   (make-posn 35 35)
                   (make-posn 45 35)))
                (make-worm
                 "right"
                 (list
                  (make-posn 25 35)
                  (make-posn 35 35)
                  (make-posn 45 35)))
                "new worm created"))
;STRATEGY: Function Composition
(define (create-worm list)
  (make-worm RIGHT list))

; replace-worm : World Worm -> World
; Replaces *only the positions* of the Worm in World w with the positions
; of the given worm. Any other Worm properties in the resulting World 
; should be the same as in the input World.
; WHERE: The Worm does not overlap with the food.
;Example:
(begin-for-test
  (check-equal? (replace-worm INITIAL-WORLD WORM2)
                (make-world
                 (make-worm 
                  "right"
                  (list 
                   (make-posn 25 35)
                   (make-posn 35 35)
                   (make-posn 45 35)))
                 (make-posn 25 85))
                "worm is replaced"))
;STRATEGY : Data Decomposition of w : World
(define (replace-worm w worm) 
  (make-world (new-worm (world-worm w) worm) (world-food w)))


; new-worm: Worm Worm -> Worm
; Will make new worm with new position keeping direction same
;;Example
(begin-for-test
  (check-equal? (new-worm WORM1 WORM2)
                (make-worm 
                 "right" 
                 (list 
                  (make-posn 25 35)
                  (make-posn 35 35)
                  (make-posn 45 35)))
                "new worm"))
;STRATEGY: Data Decomposition of w :Worm
(define (new-worm ow nw)
  (make-worm (get-dir ow) (worm-LoPosns nw)))

;get-dir: Worm -> Direction
; will return direction of worm 
;example
(begin-for-test
  (check-equal? (get-dir WORM1)
                RIGHT
                "right direction"))
;STRATEGY: data Decomposition of w: Worm 
(define (get-dir w)
  (worm-dir w))

; get-segment: Worm -> LoPosns
; Will get worm segment position
;;Example
(begin-for-test
  (check-equal? (get-segment WORM2)
                (list 
                  (make-posn 25 35)
                  (make-posn 35 35)
                  (make-posn 45 35))
                "list of segment "))
;STRATEGY: Data Decomposition of w :Worm
(define (get-segment w)
  (worm-LoPosns w))

;head-list : list -> posn
;will return first posn from list of posn 
; example
(begin-for-test 
  (check-equal? 
   (head-list (list 
                  (make-posn 25 35)
                  (make-posn 35 35)
                  (make-posn 45 35)))
   (make-posn 25 35 )
   "head of list "))
;STRATEGY : Data Decomposition on l : LoPosns
(define (head-list l)
  (first l))

;get-x: Posn -> Coordinate   
; will return x-coordinate of posn
;example
(begin-for-test
  (check-equal?
   (get-x (make-posn 12 13))
   12
   "x-coordinate"))
;STRATEGY : Data Decomposition on p : Posn
(define (get-x p)
  (posn-x p))

;get-y: Posn -> Coordinate   
; will return y-coordinate of posn
;example
(begin-for-test
  (check-equal?
   (get-y (make-posn 12 13))
   13
   "y-coordinate"))
;STRATEGY : Data Decomposition on p : Posn
(define (get-y p)
  (posn-y p))

;Alternate Data Definition
;-----------------------------------------------------------------------------
;1
;A worm is a LoPosns
;INTERP: 
;LoPosns is list of segments of the worm
;
;Example
;(define WORM1  (list (make-posn 65 45 )
;                             (make-posn 75 45 ) 
;                             (make-posn 85 45 ))))


;A world is a (make-world Worm  Dir Posn)
;INTERP: Dir is which side dir worm will move
;food is a new random
;posn where food will be displayed 

;(define-struct world (worm  dir food))
;
;Example
;(define WORLD1 (make-world WORM1  UP (make-posn 45 45)))
;
;TEMPLATE:
;world-fn : WORLD -> ???
;(define (world-fn w)
;  (...  
;   (world-worm w) ... 
;   (world-dir w)  ...
;   (world-food w) ... ))

;Pros:- To get direction we do not need helper function to decompose both
;      world and worm . we can simply decompose world to get direction
;    

;Cons :- Dir is a property of worm and when we have some change in our 
;        functionality for example two snake then whole program will change
;     :- While changing World related properties will have to update dir 
;        as well




;2
;;A worm is a (make-worm  Dir LoPosns )
;INTERP: Dir is direction where the worm is moving 
;LoPosns is position of center of each of segments of the worm
;(define-struct worm (dir LoPosns))
;
;Example
;(define WORM1 (make-worm "right" (list (make-posn 65 45 )
;                             (make-posn 75 45 ) 
;                             (make-posn 85 45 ))))
;(define WORM2 (make-worm "down" (list (make-posn 25 35 )
;                             (make-posn 35 35 ) 
;                             (make-posn 45 35 ))))
;
;TEMPLATE:
;worm-fn : WORM -> ???
;(define (worm-fn w)
;  (... (dir-fn (worm-dir w)) ... 
;       (loposns-fn (worm-LoPosns w)) ... ))
;
;
;A world is a (make-world Worm Coordinate Coordinate)
;INTERP: x ,y are the coordinates of the center of food  
;(define-struct world (worm x y))
;
;Example
;(define WORLD1 (make-world WORM1  45 45))
;(define INITIAL-WORLD (make-world (make-worm "right" (list (make-posn 95 15)))
;                                    25 85))
;
;TEMPLATE:
;world-fn : WORLD -> ???
;(define (world-fn w)
;  (...  
;   (world-worm w) ...  
;   (world-x w) ...  
;   (world-y w) ... ))

;Pros : - no need for helper function to decompose position of center of the 
;         of the food
;
;Cons :  - Structure of program is not organized . 
;        - data segregation is difficult 

;---------------------------END---------------------------------------