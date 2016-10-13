;; Problem Set 13 : defense.rkt

;; Language 
;;------------------------------------------------------------------------------

#lang racket

;; Required Libraries
;;------------------------------------------------------------------------------

(require "extras.rkt")
(require lang/posn)
(require racket/list)
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(define TIME-ON-TASK "25")


;; Provide Functions
;;------------------------------------------------------------------------------

(provide Unit<%>)
(provide StatefulWorld<%>)
(provide mk-world)
(provide mk-ally)
(provide mk-enemy)
(provide mk-merc)


;; Global Constants
;;------------------------------------------------------------------------------

(define ALLY-COLOR "green") ; color of the ally unit
(define ENEMY-COLOR "red") ; color of the enemy unit
(define BASE-COLOR "yellow") ; color of the enemy unit
(define CROSS-COLOR "black") ; color of cross-hair
(define ALLY-WIDTH 20) ; width of the ally unit
(define ALLY-HEIGHT 20) ; height of the ally unit
(define ENEMY-RADIUS 12) ; radius of the enemy unit
(define ENEMY-DIAMETER (* 2 ENEMY-RADIUS)) ; diameter of the enemy unit
(define SOLID "solid") ; represents solid shape
(define OUTLINE "outline") ; represents outline shape
(define MERC-TOGGLE-RATE 3) ; rate at which the merc unit toggles
(define ZERO 0) ; constant zero
(define MINXY ZERO) ; minimum x and y on canvas
(define CANVAS-WIDTH 400) ; width of canvas
(define CANVAS-HEIGHT 500) ; height of canvas
(define INITIAL-BASE-HEIGHT 50) ; initial height of the base
(define INITIAL-BASE-BOUNDS (list MINXY (- CANVAS-HEIGHT INITIAL-BASE-HEIGHT)
                                  CANVAS-WIDTH
                                  CANVAS-HEIGHT)) ; initial bounds of the base
(define CROSS-IN-RADIUS 5) ; radius of inner circle in cross-hair
(define CROSS-OUT-RADIUS 10) ; radius of outer circle in cross-hair
(define CROSS-TEXT-SIZE 40) ; text size in cross-hair
(define SCORE-TEXT-SIZE 15) ; score text size
(define CROSS-TEXT "+") ; text on cross hair
(define INITIAL-CROSS-POS (make-posn 0 0)) ; initial position of cross
;; represents the rendering of the ally unit
(define ALLY-UNIT (rectangle ALLY-WIDTH ALLY-HEIGHT SOLID ALLY-COLOR))

;; represents the rendering of the enemy unit
(define ENEMY-UNIT (circle ENEMY-RADIUS SOLID ENEMY-COLOR))
(define TICK-GAP-NEW-UNIT 4) ; number of ticks after new unit has to be added
(define SCORE-ALLY 20); absolute score number for ally unit
(define SCORE-ENEMY 40); absolute score number for enemy unit
(define SCORE-MERC 60); absolute score number for mercenary unit
(define SCORE-DIV 5); score divisor
(define MT-SCENE (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)); empty scene
(define FIXED-HEIGHT 50) ; fixed component in the height
(define BTN-DOWN "button-down") ;MouseEvent
(define BTN-UP "button-up") ;MouseEvent
(define BTN-MOVE "move") ;Constant representing ShapeEvent "move"
(define HT-LOWER-LIMIT 10) ; lower limit of height allowed
(define HT-UPPER-LIMIT 500) ; upper limit of height allowed
(define TYPES-OF-UNITS 3) ; total types of units
(define DEFAULT-INTERVAL 1) ; default interval for random generation
(define DEFAULT-OFFSET 0); default offset for random generation


;; Data Definitions
;;------------------------------------------------------------------------------

;; A Bound is a Coordinate
;; Represents one of x or y coordinate
;; on a axes that starts from 0,0 at the top

;; Example:
(define BOUND1 20)

;;------------------------------------------------------------------------------

;; A Bounds is a (list Bound Bound Bound Bound)
;; Interpretation: (list left top right bottom).
;; A Bounds represents a box whose left x-coordinate is at "left", whose
;; top y-coordinate is at "top", whose right x-coordinate is at "right", and 
;; whose bottom y-coordinate is at "bottom".

;; Example:
(define BOUNDS1 (list 20 20 50 50))

;; Template:
;; bounds-fn : Bounds -> ???
;;(define (bounds-fn bnds)
;;  (... (first bnds)...
;;       (second bnds) ...
;;       (third bnds) ...
;;       (fourth bnds) ...))

;;------------------------------------------------------------------------------


;; A Velocity is a Natural, representing Pixels/tick
;; in the downward direction.

;; Example:
(define VELOCITY1 19)

;; A Color is a String
;; Represents a color

;; Example:
(define BLUE "blue")

;;------------------------------------------------------------------------------


;; Interfaces
;;------------------------------------------------------------------------------

;; Represents a unit in the game.
(define Unit<%>
  (interface ()
    ; get-loc : -> Posn
    ; Returns the location of this unit as a posn.
    get-loc
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    get-color
    
    ; get-score : -> Integer
    ; Returns the current score of the unit in the current tick
    get-score
    
    ; on-tick! : -> Void
    ; EFFECT: mutates the location of the current unit
    on-tick!
    
    ; notified-from-world! : Bounds -> Void
    ; EFFECT: Updates the value of base-bounds as received from the world
    notified-from-world!
    
    ; keep-unit? : -> Boolean
    ; Checks if the unit has entered the base or not.
    ; If it has entered then the function returns false else true.
    keep-unit?
    
    ; render-unit : -> Image
    ; Renders the current unit on the canvas
    render-unit
    
    ; mouse-down! : Posn -> Void
    ; EFFECT : Changes the state of the units which fall under
    ; the range of the target Posn sent in parameter, handles mouse-down
    ; event
    mouse-down!))

;;------------------------------------------------------------------------------


;; Represents a mutable world object.
(define StatefulWorld<%>
  (interface ()
    ; on-tick! : -> Void
    ; EFFECT: mutates this world to its next state.
    on-tick!
    
    ; on-mouse! : Coordinate Coordinate MouseEvent -> Void
    ; EFFECT: mutates this world to its next state from the 
    ; given mouse parameters.
    on-mouse!
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    target-loc
    
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    get-units
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    add-unit!
    
    ; get-base-height : -> Natural
    ; Returns the height of the base, in pixels.
    get-base-height
    
    ; init-units :  -> Bounds
    ; Returns the base-bounds (Bounds) of this world
    get-bounds
    
    ; render : -> Image
    ; Renders the current stateful world on the canvas
    render
    
    ; end? : -> Boolean
    ; Checks if the world has reached the end condition
    ; on the basis of the base height
    end?))  

;;------------------------------------------------------------------------------


;; Class Implementations
;;------------------------------------------------------------------------------


;; An AllyUnit% is a (new AllyUnit% [loc Posn]
;;                           [velocity Velocity])
;; Represents an ally unit and is with us in the war
;; loc - represents the center coordinates of the unit
;; velocity - stores the velocity of this unit that is the
;; rate at which the unit moves forward (Pixels/Tick)
;; as the unit is assigned to a world, the world gets updated here
(define AllyUnit% 
  (class* object% (Unit<%>)
    (init-field loc velocity)
    
    ; local copy of base-bounds from world
    (field [base-bounds INITIAL-BASE-BOUNDS])
    
    ; score keeps track of unit score if unit enters base or
    ; is clicked
    ; WHERE : score belongs to set {0,20,-20)
    (field [score ZERO])
    
    
    ; score-not-shot : -> Integer
    ; Computes the new score of the unit when the unit is not shot but
    ; reaches the base
    (define (score-not-shot)
      (+ ZERO SCORE-ALLY))
    
    
    ; score-shot : -> Integer
    ; Computes the new score of the unit when the unit is shot
    (define (score-shot)
      (- ZERO SCORE-ALLY))
    
    
    ; entered-base? : Bounds -> Boolean
    ; INTERP: my-bounds represent the rectangular bounds of this unit
    ; Checks if the current unit is touching or overlapping the
    ; base bounds or not
    ; Strategy : Data decomposition on base-bounds my-bounds : Bounds
    (define (entered-base? my-bounds)
      (and (<= (first base-bounds) (first my-bounds))
           (>= (third base-bounds) (third my-bounds))
           (<= (second base-bounds) (fourth my-bounds))))
    
    
    ; compute-new-score : Boolean -> Integer
    ; INTERP: shot? represents if the unit has been shot or not
    ; Computes the new score of the unit for this event
    (define (compute-new-score shot?)
      (if (not shot?)
          (score-not-shot)
          (score-shot)))
    
    ; check-set-score : Bounds -> Integer
    ; If the unit has moved into the base it computes the new
    ; score for the unit else returns zero
    (define (check-set-score my-bounds)
      (if (entered-base? my-bounds)
          (compute-new-score false)
          ZERO))
    
    ; get-my-bounds : -> Bounds
    ; Returns the rectangular Bounds of the current unit
    (define (get-my-bounds)
      (list (- (posn-x loc)
               (/ ALLY-WIDTH 2))
            
            (- (posn-y loc)
               (/ ALLY-HEIGHT 2))
            
            (+ (posn-x loc)
               (/ ALLY-WIDTH 2))
            
            (+ (posn-y loc)
               (/ ALLY-HEIGHT 2))))
    
    ; target-on-unit? : Bounds Coordinate Coordinate -> Boolean
    ; Checks if the target currently is pointed on this unit or not
    (define (target-on-unit? my-bounds mx my)
      (and (<= (first my-bounds) mx (third my-bounds))
           (<= (second my-bounds) my (fourth my-bounds))))
    
    
    ; notified-from-world! : Bounds -> Void
    ; EFFECT: Updates the value of base-bounds as received from the world
    (define/public (notified-from-world! b-bounds)
      (set! base-bounds b-bounds))
    
    ; get-loc : -> Posn
    ; Returns the location of this unit as a posn
    ; Would return the posn of the center of the unit
    (define/public (get-loc)
      loc)
    
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    ; Example: Given : color = "green" , Returns : "green" 
    ; (for ally unit color returned is always going to be green)
    (define/public (get-color)
      ALLY-COLOR)
    
    ; get-score : -> Integer
    ; Gets the current score of the unit
    (define/public (get-score)
      score)
    
    ; on-tick! : -> Unit%
    ; EFFECT: updates the position of the current unit by
    ; adding the velocity to the y coordinate, and updating the
    ; score of the unit, returns itself to the caller
    ; Strategy : Data decomposition on loc : Posn
    (define/public (on-tick!)
      (set! loc (make-posn (posn-x loc) 
                           (+ (posn-y loc) velocity)))
      (set! score (check-set-score (get-my-bounds)))
      this) 
    
    ; keep-unit? : -> Boolean
    ; If the score of the unit is non-zero then it implies
    ; the unit has either reached or has been shot and must be removed
    (define/public (keep-unit?)
      (= score ZERO))
    
    ; mouse-down! : Posn -> Unit%
    ; EFFECT : Changes the state (score) of the unit if it falls under
    ; the range of the target Posn sent in parameter, handles mouse-down
    ; event, and returns itself
    ; Strategy : Data decomposition on target : Posn
    (define/public (mouse-down! target)
      (if (target-on-unit? (get-my-bounds) (posn-x target) (posn-y target))
          (set! score (compute-new-score true))
          (set! score score))
      this)  
    
    ; render-unit : Image -> Image
    ; Renders the current unit on the canvas
    ; Strategy : data decomposition on loc : Posn
    (define/public (render-unit scn)
      (place-image ALLY-UNIT (posn-x loc) (posn-y loc) scn))
    
    (super-new)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; An EnemyUnit% is a (new EnemyUnit% [loc Posn]
;;                           [velocity Velocity])
;; Represents an enemy unit and is against us in the war
;; loc - represents the center coordinates of the unit
;; velocity - stores the velocity of this unit that is the
;; rate at which the unit moves forward (Pixels/Tick)
(define EnemyUnit% 
  (class* object% (Unit<%>)
    (init-field loc velocity)
    
    ; local copy of base-bounds from world
    (field [base-bounds INITIAL-BASE-BOUNDS])
    
    ; score keeps track of unit score if unit enters base or
    ; is clicked
    ; WHERE : score belongs to set {0,40,-40)
    (field [score ZERO])
    
    ; entered-base? : Bounds -> Boolean
    ; Checks if the current unit is touching or overlapping the
    ; base bounds or not
    ; Strategy : Data decomposition on base-bounds my-bounds : Bounds
    (define (entered-base? my-bounds)
      (and (<= (first base-bounds) (first my-bounds))
           (>= (third base-bounds) (third my-bounds))
           (<= (second base-bounds) (fourth my-bounds))))
    
    
    ; score-shot : -> Integer
    ; Computes the new score of the unit when the unit is shot 
    (define (score-shot)
      (+ ZERO SCORE-ENEMY))
    
    ; score-not-shot : -> Integer
    ; Computes the new score of the unit when the unit is not shot but
    ; reaches the base
    (define (score-not-shot)
      (- ZERO SCORE-ENEMY))
    
    ; compute-new-score : Boolean -> Integer
    ; INTERP: shot? represents if the unit has been shot or not
    ; Computes the new score of the unit for this event
    (define (compute-new-score shot?)
      (if (not shot?)
          (score-not-shot)
          (score-shot)))
    
    
    ; check-set-score : Bounds -> Integer
    ; If the unit has moved into the base it computes the new
    ; score for the unit else returns zero
    (define (check-set-score my-bounds)
      (if (entered-base? my-bounds)
          (compute-new-score false)
          ZERO))
    
    
    
    ; get-my-bounds :  -> Bounds
    ; Returns the rectangular Bounds of the current unit
    (define (get-my-bounds)
      (list (- (posn-x loc)
               ENEMY-RADIUS)
            
            (- (posn-y loc)
               ENEMY-RADIUS)
            
            (+ (posn-x loc)
               ENEMY-RADIUS)
            
            (+ (posn-y loc)
               ENEMY-RADIUS)))
    
    
    ; target-on-unit? : Bounds Coordinate Coordinate -> Boolean
    ; Checks if the target currently is pointed on this unit or not
    (define (target-on-unit? my-bounds mx my)
      (and (<= (first my-bounds) mx (third my-bounds))
           (<= (second my-bounds) my (fourth my-bounds))))
    
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn
    ; Would return the posn of the center of the unit
    (define/public (get-loc)
      loc)  
    
    ; on-tick! : -> Unit%
    ; EFFECT: updates the position of the current unit by
    ; adding the velocity to the y coordinate, and updating the
    ; score of the unit, returns itself to the caller
    ; Strategy : Data decomposition on loc : Posn
    (define/public (on-tick!)
      (set! loc (make-posn (posn-x loc) 
                           (+ (posn-y loc) velocity)))
      (set! score (check-set-score (get-my-bounds)))
      this)
    
    
    ; notified-from-world! : Bounds -> Void
    ; EFFECT: Updates the value of base-bounds as received from the world
    (define/public (notified-from-world! b-bounds)
      (set! base-bounds b-bounds))
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    ; Example: Given : color = "green" , Returns : "green" 
    ; (for ally unit color returned is always going to be green)
    (define/public (get-color)
      ENEMY-COLOR)  
    
    ; get-score : -> Integer
    ; Gets the current score of the unit    
    (define/public (get-score)
      score)
    
    ; keep-unit? : -> Boolean
    ; If the score of the unit is non-zero then it implies
    ; the unit has either reached or has been shot and must be removed
    (define/public (keep-unit?)
      (= score ZERO))
    ;(not (entered-base? (get-my-bounds))))
    
    ; mouse-down! : Posn -> Unit<%>
    ; EFFECT : Changes the state of the units which fall under
    ; the range of the target Posn sent in parameter, handles mouse-down
    ; event
    ; Strategy : Data decomposition on target : Posn
    (define/public (mouse-down! target)
      (if (target-on-unit? (get-my-bounds) (posn-x target) (posn-y target))
          (set! score (compute-new-score true))
          (set! score score))
      this)
    
    ; render-unit : Image -> Image
    ; Renders the current unit on the canvas
    ; Strategy : data decomposition on loc : Posn
    (define/public (render-unit scn)
      (place-image ENEMY-UNIT (posn-x loc) (posn-y loc) scn))
    
    (super-new)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------


;; An MercUnit% is a (new MercUnit% [loc Posn]
;;                           [velocity Velocity]
;;                           [age NonNegInt])
;; Represents a mercenary unit and switches state from being
;; enemy or ally on regular intervals of time 
;; loc - represents the center coordinates of the unit
;; velocity - stores the velocity of this unit that is the
;; rate at which the unit moves forward (Pixels/Tick)
;; age - stores the number of ticks since this unit was created
;; age is used to toggle the state of the unit between enemy and
;; ally state, the default value of age is ZERO
;; age also helps in computing the color of the current unit
(define MercUnit% 
  (class* object% (Unit<%>)
    (init-field loc velocity age)
    
    ; local copy of base-bounds from world
    (field [base-bounds INITIAL-BASE-BOUNDS]) 
    
    ; score keeps track of unit score if unit enters base or
    ; is clicked
    ; WHERE : score belongs to set {0,60,-60)
    (field [score ZERO])
    
    ; entered-base? : Bounds -> Boolean
    ; Checks if the current unit is touching or overlapping the
    ; base bounds or not
    ; Strategy : Data decomposition on base-bounds my-bounds : Bounds
    (define (entered-base? my-bounds)
      (and (<= (first base-bounds) (first my-bounds))
           (>= (third base-bounds) (third my-bounds))
           (<= (second base-bounds) (fourth my-bounds))))
    
    ; score-not-shot : -> Integer
    ; Computes the new score of the unit when the unit is not shot but
    ; reaches the base    
    (define (score-not-shot)
      (if (ally-unit?)
          (+ ZERO SCORE-MERC)
          (- ZERO SCORE-MERC)))
    
    
    ; score-shot : -> Integer
    ; Computes the new score of the unit when the unit is shot    
    (define (score-shot)
      (if (ally-unit?)
          (- ZERO SCORE-MERC)
          (+ ZERO SCORE-MERC)))
    
    
    ; compute-new-score : Boolean -> Integer
    ; INTERP: shot? represents if the unit has been shot or not
    ; Computes the new score of the unit for this event
    (define (compute-new-score shot?)
      (if (not shot?)
          (score-not-shot)
          (score-shot)))
    
    ; ally-unit? :  -> Boolean
    ; Returns true if the unit is ally type else returns false
    ; Computes the type on the basis of the age of the unit
    ; If the output is even unit type is ally and it would return true
    ; else enemy and false.
    (define (ally-unit?)
      (even? (floor (/ age MERC-TOGGLE-RATE)))) 
    
    
    ; get-ally-bounds : -> Bounds
    ; Gets the bounds considering the unit is ally type
    (define (get-ally-bounds)
      (list (- (posn-x loc)
               (/ ALLY-WIDTH 2))
            
            (- (posn-y loc)
               (/ ALLY-HEIGHT 2))
            
            (+ (posn-x loc)
               (/ ALLY-WIDTH 2))
            
            (+ (posn-y loc)
               (/ ALLY-HEIGHT 2))))  
    
    ; get-enemy-bounds : -> Bounds
    ; Gets the bounds considering the unit is enemy type
    (define (get-enemy-bounds)
      (list (- (posn-x loc)
               ENEMY-RADIUS)
            
            (- (posn-y loc)
               ENEMY-RADIUS)
            
            (+ (posn-x loc)
               ENEMY-RADIUS)
            
            (+ (posn-y loc)
               ENEMY-RADIUS)))
    
    ; get-my-bounds :  -> Bounds
    ; Returns the rectangular Bounds of the current unit on the basis
    ; of the current state of the mercenary unit
    (define (get-my-bounds)
      (if (ally-unit?)
          (get-ally-bounds)
          (get-enemy-bounds)))
    
    
    ; target-on-unit? : Bounds Coordinate Coordinate -> Boolean
    ; Checks if the target currently is pointed on this unit or not
    (define (target-on-unit? my-bounds mx my)
      (and (<= (first my-bounds) mx (third my-bounds))
           (<= (second my-bounds) my (fourth my-bounds))))
    
    ; compute-new-score : Boolean -> Integer
    ; INTERP: shot? represents if the unit has been shot or not
    ; Computes the new score of the unit for this event
    (define (check-set-score my-bounds)
      (if (entered-base? my-bounds)
          (compute-new-score false)
          ZERO))
    
    ; get-loc : -> posn
    ; Returns the location of this unit as a posn
    ; Would return the posn of the center of the unit
    (define/public (get-loc)
      loc)  
    
    ; on-tick! : -> Unit%
    ; EFFECT: updates the position of the current unit by
    ; adding the velocity to the y coordinate, and updating the
    ; score of the unit, returns itself to the caller
    ; Strategy : Data decomposition on loc : Posn
    (define/public (on-tick!)
      (set! loc (make-posn (posn-x loc) 
                           (+ (posn-y loc) velocity)))
      (set! score (check-set-score (get-my-bounds)))
      (set! age (add1 age))
      this)
    
    
    ; notified-from-world! : Bounds -> Void
    ; EFFECT: Updates the value of base-bounds as received from the world
    (define/public (notified-from-world! b-bounds)
      (set! base-bounds b-bounds))
    
    
    ; get-color : -> Color
    ; Returns the color of this unit.
    ; Example: Given : color = "green" , Returns : "green" 
    ; (for ally unit color returned is always going to be green)
    (define/public (get-color)
      (if (ally-unit?)
          ALLY-COLOR
          ENEMY-COLOR))
    
    ; keep-unit? : -> Boolean
    ; If the score of the unit is non-zero then it implies
    ; the unit has either reached or has been shot and must be removed
    (define/public (keep-unit?)
      (= score ZERO))
    ;(not (entered-base? (get-my-bounds))))
    
    ; get-score : -> Integer
    ; Gets the current score of the unit    
    (define/public (get-score)
      score)
    
    ; mouse-down! : Posn -> Unit%
    ; EFFECT : Changes the state (score) of the unit if it falls under
    ; the range of the target Posn sent in parameter, handles mouse-down
    ; event, and returns itself
    ; Strategy : Data decomposition on target : Posn
    (define/public (mouse-down! target)
      (if (target-on-unit? (get-my-bounds) (posn-x target) (posn-y target))
          (set! score (compute-new-score true))
          (set! score score))
      this)
    
    ; render-unit : Image -> Image
    ; Renders the current unit on the canvas
    ; Strategy : data decomposition on loc : Posn
    (define/public (render-unit scn)
      (if (ally-unit?)
          (place-image ALLY-UNIT (posn-x loc) (posn-y loc) scn)
          (place-image ENEMY-UNIT (posn-x loc) (posn-y loc) scn)))
    
    (super-new)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; An StatefulWorld% is a (new StatefulWorld% [minvel Velocity] 
;;                           [maxvel Velocity])] [num-units NonNegInt]
;;                           [base-bounds Bounds]
;;                           [ticks-since-start NonNegInt]
;;                           
;; Represents a stateful world that changes its state at every tick
;; of the universe
;; units - represents the collection of all the units currently in
;; the world
;; minvel - minimum value of the velocity that the world can assign to 
;; a unit
;; maxvel - maximum value of the velocity that the world can assign to 
;; a unit
;; num-units - represents the number of units that the world has to be
;; initialized with, these units are randomly generated
;; base-bounds - bounds of the rectangular base at the bottom
;; ticks-since-start - number of ticks elapsed since the world was created
(define StatefulWorld% 
  (class* object% (StatefulWorld<%>)
    (init-field minvel maxvel num-units [base-bounds INITIAL-BASE-BOUNDS] 
                [ticks-since-start ZERO])
    
    ; represents the cross-hair target position that is used to shoot the untis
    ; It is initially pointed at (0,0) ,i.e., top left corner
    (field [target INITIAL-CROSS-POS])
    
    ; get-random-unit : Velocity NonNegInt -> Unit<%>
    ; INTERP: random-vel is the velocity to be assigned to the new unit that
    ; is going to be created,  rnd-num is used to pick which
    ; unit to create as it is a random number
    ; Creates a random unit and returns it.
    ; Strategy :Function Composition
    (define (get-random-unit random-vel rnd-num)
      (local( ; generates a random ally unit
             (define get-random-ally
               (mk-ally (make-posn (random-number 
                                    CANVAS-WIDTH 
                                    ALLY-WIDTH 
                                    (/ ALLY-WIDTH 2))
                                   MINXY)
                        random-vel))
             
             ; generates a random enemy unit
             (define get-random-enemy
               (mk-enemy (make-posn (random-number 
                                     CANVAS-WIDTH
                                     (* ENEMY-RADIUS 2)
                                     ENEMY-RADIUS) 
                                    MINXY)
                         random-vel))
             
             ; generates a random mercenary unit
             (define get-random-merc
               (mk-merc (make-posn (random-number
                                    CANVAS-WIDTH 
                                    ALLY-WIDTH 
                                    (/ ALLY-WIDTH 2))
                                   MINXY)
                        random-vel)))
        
        (if (= rnd-num ZERO)
            get-random-ally
            (if (= rnd-num 1)
                get-random-enemy
                get-random-merc))))
        
    ; random-unit-number : -> NonNegInteger
    ; Generates a random number between 0 and the number of 
    ; types of units that are in the game
    (define (random-unit-number)
      (random-number TYPES-OF-UNITS 
                     DEFAULT-INTERVAL
                     DEFAULT-OFFSET))
    
    ; random-velocity : -> NonNegInteger
    ; Generates a random velocity between the given min
    ; and max velocities
    (define (random-velocity)
      (+ minvel 
         (random-number 
          (add1 (- maxvel minvel))
          DEFAULT-INTERVAL
          DEFAULT-OFFSET)))
    
    ; generate-units : Natural -> Listof<Units%>
    ; EFFECT : Creates a new list of units which has units equal to 
    ; num-units and all the units are randomly generated 
    (define/public (generate-units)
      
      (local
        (; generate-units/a : ListOf<Unit<%>> -> ListOf<Unit<%>>
         ; Generates a ListOf<Unit<%>> from the num-units count
         ; the initial value of the units-so-far is empty list
         ; and as the units keep getting added to the list the
         ; count increases and the list is returned when the length
         ; of units-so-far is equal to num-units
         (define (generate-units/a units-so-far)
           (if (= num-units (length units-so-far))
               units-so-far
               (generate-units/a (append 
                                  (list 
                                   (get-random-unit 
                                    (random-velocity)
                                    (random-unit-number)))
                                  units-so-far)))))
        
        (generate-units/a '())))
          
    ; represents all the units that are currently in the game
    (field [units (generate-units)])
    
    
    ; publish-to-unit : Unit<%> -> Unit<%>
    ; Attaches the current world to the unit and returns
    ; back this unit
    (define (publish-to-unit un)
      (send un notified-from-world! base-bounds)) 
    
    ; publish-to-units :  -> Void
    ; Updates all the units by broadcasting its base-bounds
    ; in case they are modified
    (define (publish-to-units)
      (for-each
       (λ (u)
         (publish-to-unit u))
       units))
        
    ; Publish the value of base-bounds to all the units when the world
    ; is initialized with some random units
    (publish-to-units)
    
    ; update-units-tick : -> ListOf<Units%>
    ; Calls on-tick method of all the units and changes the state of
    ; thsoe units that are in the world currently
    (define (update-units-tick)
      (map (λ (u) (send u on-tick!)) units))  
    
    ; add-new-unit? : -> Boolean
    ; Checks if it is time to add a new random unit to the world
    (define (add-new-unit?)
      (if (and (> ticks-since-start ZERO)
               (= (modulo ticks-since-start TICK-GAP-NEW-UNIT) ZERO))
          #t
          #f))  
    
    ; add-update-units-tick! : -> ListOf<Unit%>
    ; EFFECT: Adds a new random unit and updates the remaining units in the list
    ; Strategy : Data decomposition on units : ListOf<Units<%>>
    (define (add-update-units-tick!)
      (update-units-tick)
      (set! units (append 
                   (list 
                    (get-random-unit 
                     (random-velocity)
                     (random-unit-number)))
                   units))
      (publish-to-unit (first units))
      units)
    
    ; units-on-tick : -> ListOf<Unit%>
    ; Changes the state of the units in the world on the basis of tick info
    ; and returns the updated list of units
    (define (units-on-tick)
      (if (add-new-unit?)
          (add-update-units-tick!)
          (update-units-tick)))
    
    ; filter-units : -> ListOf<Unit%>
    ; Removes the units which have reached the base
    (define (filter-units)
      (filter (λ (un)
                (send un keep-unit?)) units))
    
    ; get-y-center : Coordinate Coordinate -> Coordinate
    ; Gets the middle point of the two given coordinates
    (define (get-center end1 end2)
      (/ ( + end1 end2) 2))  
    
    ; get-x-center : -> Coordinate
    ; Gets the center x coordinate of the base rectangle
    ; Strategy :Data decomposition on base-bounds : Bounds
    (define (get-x-center)
      (get-center (first base-bounds) (third base-bounds)))
    
    ; get-y-center : -> Coordinate
    ; Gets the center y coordinate of the base rectangle
    ; Strategy :Data decomposition on base-bounds : Bounds
    (define (get-y-center)
      (get-center (second base-bounds) (fourth base-bounds)))
    
    
    ; base-width : -> NonNegInt
    ; Gets the current width of the base
    ; Strategy :Data decomposition on base-bounds : Bounds
    (define (base-width)
      (- (third base-bounds) (first base-bounds)))
    
    ; base-height : -> NonNegInt
    ; Gets the current height of the base
    ; Strategy :Data decomposition on base-bounds : Bounds
    (define (base-height)
      (- (fourth base-bounds) (second base-bounds)))
        
    ; base-height-from-bounds : -> NonNegInt
    ; Returns the current height of the base computed from the 
    ; bounds of the base
    ; Strategy : Data decomposition on base-bounds : Bounds
    (define (base-height-from-bounds)
      (- (fourth base-bounds) (second base-bounds)))  
    
    ; get-score : -> Integer
    ; Gets the score of the current world from the base height
    (define (get-score)
      (* (- (base-height-from-bounds) FIXED-HEIGHT) SCORE-DIV))
    
    ; render-base : -> Image
    ; Gets the rendering of the base on the canvas
    (define (render-base)
      (place-image
       (overlay 
        (text (number->string (get-score)) SCORE-TEXT-SIZE CROSS-COLOR)
        (rectangle (base-width) (base-height) SOLID BASE-COLOR))
       (get-x-center) (get-y-center) MT-SCENE))
    
    ; render-units : -> Image
    ; Renders all the units in the ListOf<Unit<%>>
    (define (render-units)
      (foldr (
              ; Unit<%> Image -> Image
              ; Calls the render unit method of the unit class
              ; and gets the rendering for that specific unit
              λ (un fin)
               (send un render-unit fin)) (render-base) units))
    
    ; render-cross-hair : -> Image
    ; Creates the rendering of the cross-hair that has to be 
    ; shown as a pointer
    (define (render-cross-hair)
      (overlay 
       (text CROSS-TEXT CROSS-TEXT-SIZE CROSS-COLOR)
       (circle CROSS-IN-RADIUS OUTLINE CROSS-COLOR)
       (circle CROSS-OUT-RADIUS OUTLINE CROSS-COLOR)))
    
    ; base-bounds-from-height : NonNegInteger -> Bounds
    ; Returns the new bounds of the base from the given height
    ; Strategy : Data decomposition on base-bounds : Bounds
    (define (base-bounds-from-height ht)
      (list (first base-bounds)
            (- CANVAS-HEIGHT ht)
            (third base-bounds)
            (fourth base-bounds)))  
    
    ; base-height-from-score : NonNegInteger -> Integer
    ; Returns the height of the base from the given score
    (define (base-height-from-score scr)
      (+ FIXED-HEIGHT (/ scr SCORE-DIV)))
    
    ; get-score-from-units :  -> Integer
    ; Gets the score from all the units in the units list
    (define (get-score-from-units)
      (+ (foldl (λ (un r)
                  (+ r (send un get-score))) ZERO units)))
    
    ; base-bounds-from-score : NonNegInteger NonNegInteger -> Bounds
    ; Gets the bounds of the base from the current score and the change
    ; in the score
    (define (base-bounds-from-score score-units current-score)
      (base-bounds-from-height 
       (base-height-from-score (+ score-units current-score)))) 
    
    ; mouse-hover! : Coordinate Coordinate -> Void
    ; EFFECT : Sets the target variable of the world
    (define (mouse-hover! mx my)
      (set! target (make-posn mx my)))
    
    
    ; mouse-btn-down! : Coordinate Coordinate -> Void
    ; EFFECT : Handles the mouse button down event and
    ; updates the list of units
    (define (mouse-btn-down! mx my)
      (set! units (map (λ (u) (send u mouse-down! target)) units))
      (set! base-bounds (base-bounds-from-score
                         (get-score-from-units)
                         (get-score)))
      (set! units (filter-units))
      (publish-to-units))
    
    
    ; get-bounds :  -> Bounds
    ; Returns the base-bounds (Bounds) of this world
    (define/public (get-bounds)
      base-bounds)
    
    ; on-tick! : -> Unit<%>
    ; EFFECT: mutates this world to its next state.
    ; Modifies the units, base-bounds and ticks-since-start
    ; and then notifies all the units of the change in base-bounds
    (define/public (on-tick!)
      (set! units (units-on-tick))
      (set! base-bounds (base-bounds-from-score 
                         (get-score-from-units)
                         (get-score)))
      (set! units (filter-units))
      (set! ticks-since-start (add1 ticks-since-start))
      (publish-to-units)
      this)
    
    ; on-mouse! : Coordinate Coordinate MouseEvent -> StatefulWorld<%>
    ; EFFECT: mutates this world to its next state from the 
    ; given mouse parameters.
    (define/public (on-mouse! mx my mev)
      (cond
        [(string=? mev BTN-DOWN) (mouse-btn-down! mx my)]
        [(string=? mev BTN-MOVE) (mouse-hover! mx my)]
        [else this])
      this)
    
    
    ; target-loc : -> posn
    ; Returns the center of the target as a posn.
    (define/public (target-loc)
      target)
    
    ; get-units : -> ListOf<Unit<%>>
    ; Returns the current units in this world.
    (define/public (get-units)
      units)
    
    ; add-unit! : Unit<%> -> Void
    ; EFFECT: adds the given unit to the world
    (define/public (add-unit! un)
      (set! units (append (list un) units))
      (publish-to-unit (first units)))
    
    ; get-base-height : -> Natural
    ; Returns the height of the base, in pixels.
    (define/public (get-base-height)
      (base-height-from-bounds))
    
    ; render : -> Image
    ; Renders the given shapes in the World along with the toolbar
    ; STRATEGY : Data decomposition on target : posn
    (define/public (render)
      (place-image (render-cross-hair) (posn-x target) 
                   (posn-y target) (render-units)))
    
    ; end? : -> Boolean
    ; Checks if the world has reached the end condition
    ; on the basis of the base height
    (define/public (end?)
      (not (< HT-LOWER-LIMIT (base-height-from-bounds) HT-UPPER-LIMIT)))
    
    (super-new)))

;;------------------------------------------------------------------------------


;; Non-Class Functions
;;------------------------------------------------------------------------------

;; mk-enemy : posn Velocity -> Unit<%>
;; Creates an enemy unit with the given parameters
;; Example : Given - posn = (make-posn 10 10), velocity = 2
;; Returns : (new EnemyUnit% [loc (make-posn 10 10)] [velocity 2]))
;; Strategy : Function Composition
(define (mk-enemy posn velocity)
  (new EnemyUnit% [loc posn] [velocity velocity]))

;;------------------------------------------------------------------------------

;; mk-ally : posn Velocity -> Unit<%>
;; Creates an ally unit with the given parameters
;; Example : Given - posn = (make-posn 10 10), velocity = 2
;; Returns : (new AllyUnit% [loc (make-posn 10 10)] [velocity 2]))
;; Strategy : Function Composition
(define (mk-ally posn velocity)
  (new AllyUnit% [loc posn] [velocity velocity]))

;;------------------------------------------------------------------------------

;; mk-merc : posn Velocity -> Unit<%>
;; Creates a mercenary unit with the given parameters
;; Example : Given - posn = (make-posn 10 10), velocity = 2
;; Returns : (new MercUnit% [loc (make-posn 10 10)] [velocity 2])
;;                          [age ZERO])
;; Strategy : Function Composition
(define (mk-merc posn velocity)
  (new MercUnit% [loc posn] [velocity velocity]
       [age ZERO]))

;;------------------------------------------------------------------------------

;; mk-world : Velocity Velocity Natural -> StatefulWorld<%>
;; Creates a world with num-units initial random units,
;; where units have the specified min and max velocity.
;; WHERE: minvel <= maxvel
;; Strategy :Function Composition
(define (mk-world maxvel minvel num-units)
  (new StatefulWorld% [minvel minvel]
       [maxvel maxvel][num-units num-units]))

;;------------------------------------------------------------------------------

;; random-number :  NonNegInt NonNegInt NonNegInt 
;; Returns a random number within given max value.
;; INTERP: max = maximum value for the random number
;; interval = how much separation between each random number needed
;; offset = how much should the random number be offset from the ends
;; WHERE: the returned coordinate is within the given max value
;; STRATEGY : Function composition
(define (random-number max interval offset)
  (+ offset (* interval (random (quotient max interval)))))

;;------------------------------------------------------------------------------

;; run : StatefulWorld% -> StatefulWorld%
;; Runs the big bang for the stateful world
;; STRATEGY : Function Composition
(define (run sw)
  (big-bang sw
            (on-tick next-world)
            (on-mouse handle-mouse)
            (on-draw draw-world)
            (stop-when end? draw-world)))

;;------------------------------------------------------------------------------

;; next-world : StatefulWorld% -> StatefulWorld%
;; Gets the next state of the world
;; Example
(begin-for-test
  (let ([WORLD1 (mk-world 3 1 3)])
    
    (check-equal? (next-world WORLD1)
                  (send WORLD1 on-tick!)
                  "Same behavior on-tick!")))

;; STRATEGY : Function Composition
(define (next-world sw)
  (send sw on-tick!))

;;------------------------------------------------------------------------------

;; draw-world : StatefulWorld% -> Image
;; Gets the rendering of the world to draw
;; Example
(begin-for-test
  (let ([WORLD1 (mk-world 3 1 3)])
    
    (check-equal? (draw-world WORLD1)
                  (send WORLD1 render)
                  "Same behavior render")))
;; STRATEGY : Function Composition
(define (draw-world sw)
  (send sw render))

;;------------------------------------------------------------------------------

; handle-mouse : StatefulWorld% Coordinate 
;                Coordinate MouseEvent -> StatefulWorld%
; Handles the mouse event that the user preforms and 
; updates the state of the world on the basis of the kind
; of the event and where the cursor is clicked.

; Example:
(begin-for-test
  (let ([WORLD1 (mk-world 3 1 3)])
    
    (check-equal? (handle-mouse WORLD1 300 300 BTN-DOWN)
                  (send WORLD1 on-mouse! 300 300 BTN-DOWN)
                  "Same behavior for handle mouse")))

; Strategy : Function Composition
(define (handle-mouse sw mx my mev)
  (send sw on-mouse! mx my mev))

;;------------------------------------------------------------------------------

;; end? : StatefulWorld% -> Image
;; Gets the rendering of the world to draw
;; if the game is ending
;; Example
(begin-for-test
  (let ([WORLD1 (mk-world 3 1 3)])
    
    (check-equal? (end? WORLD1)
                  (send WORLD1 end?)
                  "Same behavior to check end")))

;; STRATEGY : Function Composition
(define (end? sw)
  (send sw end?))

;;------------------------------------------------------------------------------
(define INITIAL-WORLD (mk-world 3 1 3))

;(run INITIAL-WORLD)

;; Testing for defence.rkt
;;------------------------------------------------------------------------------

;; Test to check end of the World either win or loose
(begin-for-test
  (let( 
       ;;Objects to be used for testing
       [WORLD1  (new StatefulWorld% [minvel 1]
                     [maxvel 3][num-units 0]
                     [base-bounds '(0 485 400 500)])]
       [WORLD-LOST  (new StatefulWorld% [minvel 1]
                         [maxvel 3][num-units 0]
                         [base-bounds '(0 4 400 500)])]
       [MERC-T (mk-merc  (make-posn 150 438) 3)]
       [MERC-T-S (mk-merc  (make-posn 150 100) 3)]
       [ADD-MERC (mk-merc (make-posn 150 10) 1)]
       [ADD-AL (mk-ally (make-posn 150 0) 2)] )
    
    ;;Test Cases
    (check-equal? (send WORLD1 on-tick!) (next-world WORLD1) "on-tick")
    (check-false (send WORLD1 end?) "not end")
    (check-false (end? WORLD1) "not the end")
    
    (send WORLD1 add-unit! ADD-MERC)
    
    (check-equal? (send WORLD1 get-base-height) 15 "height of base")
    
    (next-world WORLD1)
    (check-equal? (send ADD-MERC get-color) "green" "initial ally")
    
    (send WORLD1 on-mouse! 150 10 BTN-MOVE)
    (send WORLD1 on-mouse! 150 10 BTN-DOWN)
    (next-world WORLD1)
    
    (check-true (end? WORLD1) "not the end")
    (check-equal? (send WORLD1 get-base-height) 3 "height of base")
    (check-false (end? WORLD-LOST) "not the end")
    (check-equal? (send WORLD-LOST get-base-height) 496 "height of base")
    
    (send WORLD-LOST add-unit! ADD-AL)
    (next-world WORLD-LOST)
    
    (check-equal? (send WORLD-LOST get-base-height) 500 "height of base")
    (check-true (end? WORLD-LOST) "The end")))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;;Test for each units.
(begin-for-test
  (let (
        ;;Objects to be used for testing
        [WORLD (mk-world 3 1 3)]
        [ALLY-T (mk-ally (make-posn 80 430) 2)]
        [MERC-T (mk-merc  (make-posn 150 406) 3)]
        [ALLY-T-S (mk-ally (make-posn 80 100) 1)]
        [MERC-T-S (mk-merc  (make-posn 150 100) 1)]
        [ENE-T-S (mk-enemy (make-posn 200 100 ) 1)]
        [ENE-T (mk-enemy (make-posn 200 426 ) 1)] )
    
    ;;Test Cases
    (check-equal? (send WORLD get-base-height) 50 "height of base")
    (check-equal? (send WORLD get-bounds) '(0 450 400 500) "bounds of world")
    (check-equal? (send ALLY-T get-loc) (make-posn 80 430) "loc of ally")
    (check-equal? (send MERC-T get-loc) (make-posn 150 406) "loc of mercenary")
    (check-equal? (send ENE-T get-loc) (make-posn 200 426) "loc of enemy")
    (check-equal? (send ALLY-T get-color) "green" "color of ally")
    (check-equal? (send MERC-T get-color) "green" "initial ally")
    (check-equal? (send ENE-T get-color) "red" "color of ally")
    
    (send WORLD add-unit! ENE-T)
    (send WORLD add-unit! MERC-T)
    (send WORLD add-unit! ALLY-T)
    
    (check-equal? (length (send WORLD get-units)) 6 "number of units ")
    (check-equal? (send (first (send WORLD get-units)) get-loc)
                  (make-posn 80 430) "added to world")
    (check-equal? (send (second (send WORLD get-units)) get-loc)
                  (make-posn 150 406) "added to world")
    (check-equal? (send (third (send WORLD get-units)) get-loc)
                  (make-posn 200 426) "added to world")
    
    (send WORLD on-tick!)
    
    (check-equal? (send (first (send WORLD get-units)) get-loc)
                  (make-posn 80 432) "added to world")
    (send WORLD on-tick!)
    (send WORLD on-tick!)
    
    (check-equal? (send MERC-T get-color) "red" "now red")
    (check-equal? (send ALLY-T get-score ) 0 "score not assigned to ally")
    
    (send WORLD on-tick!)
    (send WORLD on-tick!)
    
    (check-equal? (send WORLD get-base-height) 54 "new base height")
    (check-equal? (send ALLY-T get-score ) 20 "score assigned to ally")
    
    (send WORLD on-tick!)
    (send WORLD on-tick!)
    (send WORLD on-tick!)
    
    (check-equal? (send WORLD get-base-height) 46 "new base height")
    (check-equal? (send ENE-T get-score ) -40 "score assigned to enemy")
    
    (send WORLD on-tick!)
    (send WORLD on-tick!)  
    (send WORLD on-tick!)
    (send WORLD on-tick!)
    
    (check-equal? (send MERC-T get-loc) (make-posn 150 442) "mercenary")
    (check-equal? (send MERC-T get-color) "green" "now green")
    (check-equal? (send WORLD get-base-height) 34 "new base")
    (check-equal? (send WORLD get-bounds) '(0 466 400 500) "bounds of world")
    (check-equal? (send MERC-T get-score ) -60 "score")
    (check-equal? (length (send WORLD get-units)) 5 "number of units now ")
    
    (next-world WORLD )
    
    (check-equal? (length (send WORLD get-units)) 6 "number of units  ")
    
    (send WORLD add-unit! ALLY-T-S)
    (send WORLD on-mouse! 80 100 BTN-MOVE)
    
    (check-equal? (send WORLD target-loc) (make-posn 80 100) "posn of target")
    
    (next-world WORLD )
    (send WORLD on-mouse! 80 100 BTN-DOWN)
    
    (check-equal? (send ALLY-T-S keep-unit?) #f "false if hover+btn down")
    (check-equal? (length (send WORLD get-units)) 6 "after removed")
    (check-equal? (send WORLD get-base-height) 30 "new base")
    
    (send WORLD add-unit! MERC-T-S)
    (send WORLD on-mouse! 150 100 BTN-MOVE)
    (send WORLD on-mouse! 150 100 BTN-DOWN)
    (next-world WORLD)
    
    (check-equal? (send WORLD get-base-height) 18 "new base")
    (check-equal? (send WORLD target-loc) (make-posn 150 100 ) "pos of target")
    (check-equal? (send MERC-T-S keep-unit?) #f "false if hover + btn down")
    (check-equal? (send WORLD get-bounds)  '(0 482 400 500) "new bound")
    (check-equal? (length(send WORLD get-units)) 6 "units")
   
    (send WORLD add-unit! ENE-T-S)
   
    (check-equal? (send WORLD get-base-height) 18 "new height of base")
    
    (next-world WORLD)
    
    (send WORLD on-mouse! 200 100 BTN-MOVE)
    (send WORLD on-mouse! 200 100 BTN-DOWN)
    
    (check-equal? (send WORLD get-base-height) 26 "new height of base")))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Test for Merc Unit
(begin-for-test
  (let (
        ;;Objects to be used for testing
        [WORLD1 (mk-world 3 1 3)]
        [MERC-T (mk-merc  (make-posn 150 438) 3)]
        [MERC-T-S (mk-merc  (make-posn 150 100) 3)])
    
    ;;Test Cases
    (check-equal? (send WORLD1 get-base-height) 50 "height of base")
    
    (send WORLD1 add-unit! MERC-T)
    
    (check-equal? (length(send WORLD1 get-units)) 4 "units")
    (check-equal? (send (first (send WORLD1 get-units)) get-loc)
                  (make-posn 150 438) "added to world")
    (check-true (send MERC-T keep-unit?) "Still remain same")
    
    (send WORLD1 on-tick!)
    
    (check-equal? (length(send WORLD1 get-units)) 3 "units")
    (check-equal? (send MERC-T get-score ) 60 "score")
    (check-equal? (send MERC-T get-color ) "green" "color of ally")
    (check-equal? (send WORLD1 get-base-height) 62 "new height of base")
    (check-false (send MERC-T keep-unit?)"Reached base so will move out")
    
    (send WORLD1 on-tick!)
    (send WORLD1 add-unit! MERC-T-S)
    
    (check-equal? (length(send WORLD1 get-units)) 4 "units")
    (check-equal? (send (first (send WORLD1 get-units)) get-loc)
                  (make-posn 150 100) "added to world")
    (check-equal? (send MERC-T-S get-color ) "green" "color of mercenary")
    
    (send WORLD1 on-tick!)
    (send WORLD1 on-tick!)
    (send WORLD1 on-mouse! 150 100 BTN-UP)
    
    (check-equal? (length(send WORLD1 get-units)) 4 "number of units")
    (check-equal? (send MERC-T-S get-score ) 0 "before score is assigned")
    (check-equal? (send WORLD1 get-base-height) 62 "height remains same")
    
    (send WORLD1 on-tick!)
    
    (check-equal? (send MERC-T-S get-color ) "red" "color of enemy")
    
    (send WORLD1 on-mouse! 250 300 BTN-DOWN)
    
    (check-equal? (length(send WORLD1 get-units)) 5 "number of units")
    (check-equal? (send MERC-T-S get-score ) 0 "before score is assigned")
    (check-equal? (send WORLD1 get-base-height) 62 "height remains same")
    
    (send WORLD1 on-tick!)
    (send WORLD1 on-mouse! 150 100 BTN-MOVE)
    (send WORLD1 on-mouse! 150 100 BTN-DOWN)
    
    (check-equal? (length(send WORLD1 get-units)) 4 "total number of units ")
    (check-equal? (send MERC-T-S get-score ) 60 "score of mercenary unit")
    (check-equal? (send WORLD1 get-base-height) 74 "new height of base")))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Test Render
(begin-for-test
  (let(
        ;;Objects to be used for testing
       [WR (mk-world 3 1 0)]
       [ALLY-T-S (mk-ally (make-posn 80 100) 1)]
       [MERC-T-S (mk-merc  (make-posn 150 100) 1)]
       [MRENE (new MercUnit% [loc (make-posn 120 120)] [velocity 1]
                   [age 5])]
       [ENE-T-S (mk-enemy (make-posn 200 100 ) 1)])
    
    ;; Test Cases
    (send WR add-unit! ALLY-T-S)
    (send WR add-unit! MERC-T-S)
    (send WR add-unit! ENE-T-S)
    (send WR add-unit! MRENE)
    
    (check-equal?
     (draw-world WR)
     (place-image 
      (overlay 
       (text CROSS-TEXT CROSS-TEXT-SIZE CROSS-COLOR)
       (circle CROSS-IN-RADIUS OUTLINE CROSS-COLOR)
       (circle CROSS-OUT-RADIUS OUTLINE CROSS-COLOR))
      0 0
      (place-image 
       ALLY-UNIT 80 100
       (place-image
        ENEMY-UNIT 200 100
        (place-image 
         ALLY-UNIT 150 100
         (place-image
          ENEMY-UNIT  120 120
          (place-image
           (overlay 
            (text (number->string 0) SCORE-TEXT-SIZE CROSS-COLOR)
            (rectangle 400 50  SOLID BASE-COLOR))
           200 475 MT-SCENE)))))))
    "render of the world with all elements"))

;;------------------------------------------------------------------------------


;; Alternate Design
;; -----------------------------------------------------------------------------

;; Explanation of current data design

;; In the current design we have implemented a publisher 
;; subscriber design where Unit is the subscriber and 
;; World is the Publisher. We have not implemented separate
;; interfaces for this but the functionality emulates this
;; design style. 

;; Unit contains base-bounds from the World and when it is
;; first created it gets the INITIAL-BASE-BOUNDS which is the
;; value of the bounds when the world is initialized, but as
;; soon as its creation the unit is published with the bounds
;; of the world when it is added to the list of units of the
;; world.

;; When the state of the unit changes it maintains a score
;; variable inside it which after the tick or after being shot
;; is read by the world and all the scores are summed and
;; added to the current score to compute the new height of the
;; base. Once this new height is computed the base then
;; publishes this base-bounds value to all the units to bring
;; back the consistency.

;; We reached this design after lot of refactoring and implementing
;; various designs and ultimately felt that this was the most 
;; efficient and apt design as here the world is always controlling
;; its state and unit cannot modify any property of the world which
;; makes it more stable, as unit is always going to be the part of
;; the world and not other way around.

;; The flaw with this design is that we are doing a pull for score
;; every time after the tick or mouse down.

;; The alternate data definition that we can use for this is as follows-


;; -----------------------------------------------------------------------------
;; Alternate Data Definition

;; One design that we considered was having a reference of the world
;; in every unit where unit is initialized with empty world and as soon as
;; its assigned to the world, the unit's world variable would be set to the
;; world sent from the world itself. After this we could have written a get
;; function for the base-bounds in the world that that could be used by the
;; unit to update the local copy of the base-bounds. After every tick or click
;; this base-bounds local copy could be updated by the world just by doing 
;; a push or publish as in this case all the units are subscribed to the world.


;; But, in this case we are giving access to the unit to do a pull on 
;; base-bounds which can lead to inconsistency. During our implementation we 
;; tried to update the base-bounds of the world after encountering each unit 
;; and then publishing new  base-bounds to every unit before the tick had 
;; actually finished, which lead to the inconsistency in the value of the 
;; base-bounds. But in that case we would not need to maintain a score in the 
;; unit as we would publish the updated base-bounds immediately after 
;; encountering the unit. The unit could be later removed by checking its 
;; bounds in another iteration.


;; An MercUnit% is a (new MercUnit% [loc Posn]
;;                           [velocity Velocity]
;;                           [age NonNegInt]
;;                           [st-world StatefulWorld<%>])
;; Represents a mercenary unit and switches state from being
;; enemy or ally on regular intervals of time 
;; loc - represents the center coordinates of the unit
;; velocity - stores the velocity of this unit that is the
;; rate at which the unit moves forward (Pixels/Tick)
;; age - stores the number of ticks since this unit was created
;; age is used to toggle the state of the unit between enemy and
;; ally state, the default value of age is ZERO
;; age also helps in computing the color of the current unit
;; st-world - keeps the reference of the container world in the unit

;; We would not need to keep the local score here.


;; Additionally, instead of keeping bounds in the world we could just
;; keep the top y axis of the base as that is the only thing that we are
;; interacting with in this application. The benefit is that we dont have
;; to implement functions like get-height-from-bounds and get-bounds-from-height
;; functions here but then keeping bounds makes the code more extensible
;; if the position of the base is changed later on, like its moved up or
;; its width is not equal to the world width and is lesser.

;; An StatefulWorld% is a (new StatefulWorld% [minvel Velocity] 
;;                           [maxvel Velocity])] [num-units NonNegInt]
;;                           [base-top Coordinate]
;;                           [ticks-since-start NonNegInt]
;;                           
;; Represents a stateful world that changes its state at every tick
;; of the universe
;; units - represents the collection of all the units currently in
;; the world
;; minvel - minimum value of the velocity that the world can assign to 
;; a unit
;; maxvel - maximum value of the velocity that the world can assign to 
;; a unit
;; num-units - represents the number of units that the world has to be
;; initialized with, these units are randomly generated
;; base-top - Represents the top y coordinate of the base which can be used
;; to compute the height pr score
;; ticks-since-start - number of ticks elapsed since the world was created