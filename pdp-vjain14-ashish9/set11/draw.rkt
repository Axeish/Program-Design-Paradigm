;; Problem Set 11 : draw.rkt

;; Language 
;;------------------------------------------------------------------------
#lang racket

;; Required Libraries
;;------------------------------------------------------------------------

(require "extras.rkt")
(require lang/posn)
(require racket/list)
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(define TIME-ON-TASK 18) ; hours spent

;; Provide Functions
;;------------------------------------------------------------------------

(provide INITIAL-WORLD)
(provide handle-mouse)
(provide Shape<%>)
(provide get-world-shapes)
(provide create-rectangle)
(provide create-circle)

;; Constants
;;------------------------------------------------------------------------

(define POINTER-SEL "p")
(define CIRCLE-SEL "c")  
(define RECTANGLE-SEL "r")
(define BTN-DOWN "button-down") ;MouseEvent
(define BTN-UP "button-up") ;MouseEvent
(define BTN-DRAG "drag") ;MouseEvent
(define RESIZE "resize") ;Constant representing ShapeEvent "resize"
(define MOVE "move") ;Constant representing ShapeEvent "move"
(define NONE "none") ;Constant representing ShapeEvent "none"
(define EMPTY-SCENE (empty-scene 700 700)) ;Image representing empty-scene
(define ZERO 0) ; represents constant 0
(define P-LIMIT 20) ; represents constant 0
(define R-LIMIT 40) ; represents constant 0
(define C-LIMIT 60) ; represents constant 0
(define TXT-COL-INV "white") ; represents color of text in selected tool box
(define TXT-COL-NORM "black") ; represents color of text in unselected tool box
(define SOLID "solid") ; represents solid filled color
(define OUTLINE "outline") ; represents outline state of a shape
(define TEXT-SIZE 10) ; size of text in toolbox
(define TXT-BOX-HT 20) ; height of toolbox tool
(define TXT-BOX-WD 20) ; width of toolbox tol
(define CIRCLE-BOUND 2) ; control region bound of circle
(define RECTANGLE-BOUND 5) ; control region bound of rectangle
(define OPACITY 127) ; opacity number for new shape
(define OP-COL "red") ; opacity color for new shape
(define MD-PNT-DIV 2) ; mid point divisor
(define LEFT "left") ; represents string "left"
(define TOP "top") ; represents string "top"

;;------------------------------------------------------------------------

;; A Selection is one of -
;; - "p" 
;; - "c"
;; - "r"
;; Interpretation: Reperesents the current selection in the world
;; it could be a pointer, circle or a rectangle

;; <Selection Predicates> : Selection -> Boolean
;; Returns True if sel is the Selection indicated by the function name.
;; Strategy: Function Composition
(define (pointer-sel? sel) (string=? POINTER-SEL sel))
(define (circle-sel? sel) (string=? CIRCLE-SEL sel)) 
(define (rectangle-sel? sel) (string=? RECTANGLE-SEL sel))

;; EXAMPLES:

(define SEL-1 "p")
(define SEL-2 "r")
(define SEL-3 "c")

;; Template:
;; selection-fn: Selection -> ???
;; (define (selection-fn sel)
;;  (cond
;;    [(pointer-sel? sel) ...]
;;    [(circle-sel? sel) ...]
;;    [(rectangler-sel? sel) ...]))

;;------------------------------------------------------------------------

;; A ShapeEvent is one of -
;; - "resize" 
;; - "move"
;; - "none"
;; Interpretation: Reperesents the two events that can be attached to a shape,
;; resizing or moving, none specifies that shape is not being modified

;; <ShapeEvent Predicates> : ShapeEvent -> Boolean
;; Returns True if se is the ShapeEvent indicated by the function name.
;; Strategy: Function Composition

(define (resize? se) (string=? RESIZE se))
(define (move? se) (string=? MOVE se))
(define (none? se) (string=? NONE se))


(define SE-1 "resize")
(define SE-2 "move")
(define SE-3 "none")

;; Template:
;; selection-fn: ShapeEvent -> ???
;; (define (selection-fn se)
;;  (cond
;;    [(resize? sel) ...]
;;    [(move? sel) ...]
;;    [(none? sel) ...]))

;;------------------------------------------------------------------------

;; A CircleCoords is one of -
;; - 'x
;; - 'y
;; Interpretation: Represents the center coordinate names of the circle
;; 'x for x axis and 'y for y axis

(define X 'x)
(define Y 'y)

;; Template:
;; circle-coords-fn: CircleCoords -> ???
;; (define (circle-coords-fn cc)
;;  (cond
;;    [(symbol=? cc 'x) ...]
;;    [(symbol=? cc 'y) ...]))

;;------------------------------------------------------------------------


;; A RectangleCoords is one of -
;; - 'l
;; - 't
;; - 'r
;; - 'b
;; Interpretation: Represents the four vertices of a rectangle

(define L 'l)
(define T 't)
(define R 'r)
(define B 'b)

;; Template:
;; rectangle-coords-fn: RectangleCoords -> ???
;; (define (rectangle-coords-fn rc)
;;  (cond
;;    [(symbol=? rc 'l) ...]
;;    [(symbol=? rc 't) ...]
;;    [(symbol=? rc 'r) ...]
;;    [(symbol=? rc 'b) ...]))

;;------------------------------------------------------------------------

;; A BoundingBox is a (list Coordinate Coordinate Coordinate Coordinate)
;; Interpretation: (list left top right bottom).
;; A BoundingBox represents a box whose left x-coordinate is at "left", whose
;; top y-coordinate is at "top", whose right x-coordinate is at "right", and 
;; whose bottom y-coordinate is at "bottom".

;; Example:
(define Box1 (list 20 20 50 50))
  
;; Template:
;; boundingbox-fn : BoundingBox -> ???
;;(define (boundingbox-fn bb)
;;  (... (first bb)...
;;       (second bb) ...
;;       (third bb) ...
;;       (fourth bb ) ...))
  
  
;;------------------------------------------------------------------------

; A World<%> represents an interface that is inherited by the World%
(define IWorld<%>
  (interface ()
    
    ; next-world : -> World%
    ; Returns the next world which in this case would be the current world
    ; itself as the state of our world changes only with mouse event
    ; and not time tick
    next-world
    
    ; mouse-handler : Coordinate Coordinate MouseEvent -> World%
    ; Handles the incoming mouse event and dispatches the task to other
    ; sub functions on the basis of kind of event, where mx is x coordinate,
    ; my is y coordinate and mev is the mosue event
    mouse-handler
    
    
    ; get-shapes : -> ListOf<Shape<%>>
    ; Gets the shapes field of the current world object
    get-shapes
    
    ; get-selection : -> Selection
    ; Gets the Selection field of the current world object
    get-selection
    
    ; get-mouse-ev : -> MouseEvent
    ; Gets the ex-mouse-ev field of the current world object
    get-mouse-ev
    
    ; get-start-posn : -> Posn
    ; Gets the start-posn field of the current world object
    get-start-posn
    
    ; get-currentt-posn : -> Posn
    ; Gets the current-posn field of the current world object
    get-current-posn
    
    ; render : -> Image
    ; Renders the world
    render))

;;------------------------------------------------------------------------

; A World% is a (new World% [shapes ListOf<Shape<%>>] [selection Selection] 
;                           [ex-mouse-ev MouseEvent] [start-posn Posn]
;                           [current-posn Posn])
; Represents the world that is being looked at currently
; INTERP: 
; shapes contain the list of shapes that are currectly drawn in the world
; selection represents the current selection that could be pointer, circle or
; rectangle
; ex-mouse-ev represents the last mouse event that the user had triggered
; start-posn represents the starting point of the mouse event, for instance if
; a click and drag was initiated then the start-posn is the posn where the user
; had clicked
; current-posn stores the current click state

(define World% 
  (class* object% (IWorld<%>)
  (init-field shapes selection ex-mouse-ev start-posn current-posn)
     
  
  ; next-world : -> World%
  ; Returns the next world which in this case would be the current world
  ; itself as the state of our world changes only with mouse event
  ; and not time tick
  ; EXAMPLES:
  ; GIVEN : TEST-WORLD , RETURNS : TEST-WORLD
  (define/public (next-world)
    (get-current-world))
  
  ; mouse-handler : Coordinate Coordinate MouseEvent -> World%
  ; Handles the incoming mouse event and dispatches the task to other
  ; sub functions on the basis of kind of event, where mx is x coordinate,
  ; my is y coordinate and mev is the mosue event
  ; STRATEGY: Data decomposition on mev : MouseEvent
  (define/public (mouse-handler mx my mev)
    (cond
    [(string=? mev BTN-UP) (btn-up mx my)]
    [(string=? mev BTN-DOWN) (btn-dwn mx my)]
    [(string=? mev BTN-DRAG) (btn-drag mx my)]
    [else (get-current-world)]))
  
  
  ; next-world : -> World%
  ; Returns the current state of the world
  ; EXAMPLES:
  ; GIVEN : TEST-WORLD , RETURNS : TEST-WORLD    
  (define (get-current-world)
    (new World% [shapes shapes]
               [selection selection]
               [ex-mouse-ev ex-mouse-ev]
               [start-posn start-posn]
               [current-posn current-posn]))
    
  ; pointer? : Coordinate Coordinate -> Boolean
  ; Check if the mouse has been clicked in the 
  ; pointer region of the toolbox where mx is the x
  ; coordinate and my is the y coordinate
  ; EXAMPLES:
  ; GIVEN : mx = 5 , my = 10 ; RETURN : true
  (define (pointer? mx my)
    (and (< ZERO mx P-LIMIT)
         (< ZERO my P-LIMIT)))
  
    
  ; rectangle? : Coordinate Coordinate -> Boolean
  ; Check if the mouse has been clicked in the 
  ; rectangle region of the toolbox where mx is the x
  ; coordinate and my is the y coordinate
  ; EXAMPLES:
  ; GIVEN : mx = 5 , my = 30 ; RETURN : true
  (define (rectangle? mx my)
    (and (< ZERO mx P-LIMIT)
         (< P-LIMIT my R-LIMIT)))
  
    
  ; circle? : Coordinate Coordinate -> Boolean
  ; Check if the mouse has been clicked in the 
  ; rectangle region of the toolbox where mx is the x
  ; coordinate and my is the y coordinate
  ; EXAMPLES:
  ; GIVEN : mx = 5 , my = 50 ; RETURN : true
  (define (circle? mx my)
    (and (< ZERO mx P-LIMIT)
         (< R-LIMIT my C-LIMIT)))
  
  
  ; create-world : ListOf<Shape<%>> Selection MouseEvent Posn Posn ->world
  ; Creates a new world with the specified fields which could be different
  ; from the fields of the current object
  (define (create-world shps sel mev st-pos cr-pos)
    (new World% [shapes shps] [selection sel]
               [ex-mouse-ev mev]
               [start-posn st-pos]
               [current-posn cr-pos]))
  
  
  ; btn-dwn : Coordinate Coordinate -> world%
  ; Gets the state of the world on the button down  mouse event
  (define (btn-dwn mx my)
    (if (shape-pick? mx my)
        (create-world shapes (update-sel mx my) BTN-DOWN (make-posn mx my)
                     (make-posn mx my))
        (btn-dwn-by-sel mx my BTN-DOWN)))
     
  
  ; update-new-shape? :-> Boolean
  ; Checks if there is a new shape at the top of the list of
  ; shapes and it has just been added and is being resized now
  ; WHERE: Length of shapes > 0
  ; STRATEGY : Data decomposition on shapes : ListOf<Shapes<%>> 
  (define (update-new-shape?)
    (send (first shapes) get-new-s))
    
  
  ; btn-dwn-by-sel : Coordinate Coordinate MouseEvent -> World%
  ; Checks which Selection is currently selected and decides on
  ; basis of that how world needs to be modified
  ; STRATEGY : Data decomposition on selection : Selection
  (define (btn-dwn-by-sel mx my mev)
    (cond
      [(pointer-sel? selection ) (create-world 
                                  (update-shapes mx my BTN-DOWN true) 
                                  selection BTN-DOWN (make-posn mx my)
                                  (make-posn mx my))]
      
      [else (create-world (add-shape mx my BTN-DOWN) 
                          selection BTN-DOWN (make-posn mx my)
                          (make-posn mx my))]))
    
  
  ; get-world-valid-drag : Coordinate Coordinate -> World%
  ; Gets the new world if the drag performed by the user is a
  ; valid one
  (define (get-world-valid-drag mx my)
    (if (update-new-shape?)
            (create-world (update-shapes mx my BTN-DRAG false) 
                    selection
                    BTN-DRAG 
                    start-posn
                    (make-posn mx my))
            
            (create-world (update-shapes mx my BTN-DRAG true) 
                    selection
                    BTN-DRAG 
                    start-posn
                    (make-posn mx my))))
    
    
  ; btn-drag : Coordinate Coordinate -> World%
  ; Returns a new World% when MouseEvent is drag 
  (define (btn-drag mx my)
    (if (valid-drag? mx my) 
        (get-world-valid-drag mx my)
        (create-world shapes selection 
                    BTN-DRAG start-posn
                    (make-posn mx my))))
  
  
  ; update-sel : Coordinate Coordinate -> Selection
  ; Changes the value of selection according to the mouse click coordinates
  ; STRATEGY : Data decomposition on selection : Selection 
  (define (update-sel mx my)
    (cond
         [(pointer? mx my) POINTER-SEL]
         [(rectangle? mx my) RECTANGLE-SEL]
         [(circle? mx my) CIRCLE-SEL]))
  
    
  ; btn-up : Coordinate Coordinate -> World%
  ; Returns mouse event when Mouse button is released
  ; STRATEGY : Data decomposition on ex-mouse-ev : MouseEvent
  (define (btn-up mx my)
    (cond
      
      [(string=? ex-mouse-ev BTN-DOWN) 
       (create-world (update-shapes mx my BTN-UP true) 
                     selection BTN-UP
                     start-posn current-posn)]
      [(string=? ex-mouse-ev BTN-DRAG) 
       (create-world (update-shapes mx my BTN-UP true) 
                     selection BTN-UP (make-posn mx my)
                     (make-posn mx my))]
      [else (get-current-world)]))
         
  ; shape-pick? : Coordinate Coordiante -> Boolean 
  ; Checks if the user has clicked on any of the toolbar Selection area
  (define (shape-pick? mx my)
    (or (pointer? mx my)
        (circle? mx my)
        (rectangle? mx my)))

    
  ; valid-drag? Coordinate Coordiante -> Boolean
  ; Returns True if MouseEvent is not BTN-UP and mouse coordinate do not 
  ; overlap Selection icons  
  ; STRATEGY : Data decompositon on start-posn : Posn
  (define (valid-drag? mx my)
    (and (not (shape-pick? (posn-x start-posn) (posn-y start-posn)))
         (not (string=? ex-mouse-ev BTN-UP)))) 
  
  
  
  ; add-shape : Coordinate Coordinate MouseEvent -> ListOf<Shape<%>>
  ; Add new shape to the list of shapes according to the selection
  ; If the selection is pointer than returns shapes as it is else
  ; appends the shape to the first of ListOf<Shape<%>>
  ; WHERE : Length shapes > 0
  ; EXAMPLES: GIVEN : selection rectangle , RETURN : ListOf<SHAPES<%>>
  ; with a rectangle at the first index
    (define (add-shape mx my mev)
      (local (
              ; get-shape : -> MayBe<Shape<%>>
              ; Gets the new shape if the selection is circle or rectangle
              ; else returns false
              (define get-shape
                (cond
                  [(circle-sel? selection) 
                   (create-circle-new (make-posn mx my) ZERO)]
                  [(rectangle-sel? selection) 
                   (send (create-rectangle-new (list mx my mx my))
                         set-pos-from mx my)])))
        ; - IN -
        (cons get-shape shapes)))
  
    
  ; update-shapes : Coordinate Coordinate MouseEvent Boolean-> ListOf<Shape<%>>
  ; Updates the Shapes field on the basis of the mouse click event
  ; where mx is the x cordinate of mouse click and my is the y-coordinate
  ; mev is the mouse event and all? signifies if the first shape has to be
  ; updated or all shapes in the list
  ; EXAMPLES:
  ; GIVEN : all? = #t ; RETURN : ListOf<Shapes<%>> with all the shapes updated
  ; GIVEN : all? = #f ; RETURN : ListOf<Shapes<%>> with first shape updated
  (define (update-shapes mx my mev all?)
    (if all?
        (update-all-shapes mx my mev)
        (update-first-shape mx my mev)))
  
  
  ; update-all-shapes : Coordinate Coordinate MouseEvent -> ListOf<Shape<%>>
  ; Updates all the shapes in the list of shapes
  (define (update-all-shapes mx my mev)
    (map (
          ; Shape<%> -> Shape<%>
          ; Calls the mouse handler on each shape with given x y coordiantes
          ; and the mouseevent and returns the updated shape
          lambda (shp)
           (send shp handle-mouse mx my mev)) shapes))
  
    
  ; update-first-shape : Coordinate Coordinate MouseEvent -> ListOf<Shape<%>>
  ; Updates the first shape and appends the rest of shapes as it is
  ; WHERE : Length shapes > 0
  ; STRATEGY : Data decomposition on shapes : ListOf<Shapes<%>>
  (define (update-first-shape mx my mev)
    (cons (send (first shapes) handle-mouse mx my mev)
                     (rest shapes)))
  
    ; get-shapes : -> ListOf<Shape<%>>
    ; Gets the shapes field of the current world object
    (define/public (get-shapes)
      shapes)
    
    ; get-selection : -> Selection
    ; Gets the Selection field of the current world object
    (define/public (get-selection)
      selection)
    
    ; get-mouse-ev : -> MouseEvent
    ; Gets the ex-mouse-ev field of the current world object
    (define/public (get-mouse-ev)
      ex-mouse-ev)
    
    ; get-start-posn : -> Posn
    ; Gets the start-posn field of the current world object
    (define/public (get-start-posn)
      start-posn)
    
    ; get-current-posn : -> Posn
    ; Gets the current-posn field of the current world object
    (define/public (get-current-posn ) 
      current-posn)
    
    ; render : -> Image
    ; Renders the given shapes in the World along with the toolbar
    (define/public (render)
      (overlay/align LEFT TOP
                     (render-tools)
                     (render-shapes)))
    
    ; render-tools : -> Image
    ; Renders the toolbar (selections) on the top left of the screen
  (define (render-tools)
    (above (render-text-block POINTER-SEL)
           (render-text-block RECTANGLE-SEL)
           (render-text-block CIRCLE-SEL)))
  
  ; render-text-block : String -> Image
  ; Renders one text block (selection)
  (define (render-text-block txt)
    (if (string=? selection txt)
        (text-box-selected txt)
        (text-box-unselected txt)))
  
  ; text-box-selected : String -> Image
  ; Gets the selected text box image which is rectangle
  ; with black background and white text
  (define (text-box-selected txt)
    (overlay (text txt TEXT-SIZE TXT-COL-INV)
             (rectangle TXT-BOX-WD TXT-BOX-HT SOLID TXT-COL-NORM)))
    
  ; text-box-unselected : String -> Image
  ; Gets the un-selected text box image which is rectangle
  ; with white background and black text
  (define (text-box-unselected txt)
    (overlay (text txt TEXT-SIZE TXT-COL-NORM)
             (rectangle TXT-BOX-WD TXT-BOX-HT OUTLINE TXT-COL-NORM)))
   
  ; render-shapes : -> Image
  ; Renders all the shapes in the ListOf<Shape<%>>
  (define (render-shapes)
    (foldr (
           ; Shape<%> Image -> Image
           ; Calls the render shape method of the shape class
           ; and gets the rendering for that specific image
           lambda (shp fin)
            (send shp render-shape fin)) EMPTY-SCENE shapes))
  (super-new)))



;;------------------------------------------------------------------------


; A Shape<%> represents an interface that is inherited by the various
; geometrical shapes, circle and rectangle in this case.

(define Shape<%>
  (interface ()
       
    ; get-new-s : -> Boolean
    ; Returns true if the shape is new else false
    get-new-s
        
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the given shape which is the most
    ; fitting rectangle in which the shape fits perfectly
    get-bounds    
    
    ; handle-mouse : -> Shape<%>
    ; Handles the mouse event on the current shape Shape<%>
    handle-mouse
    
    ; render-shape : -> Shape<%>
    ; Gets the rendering of the current shape
    render-shape
    
    ; get-event : ->ShapeEvent
    ; returns ShapeEvent of the shape
    get-event
    
    ; get-pos : ->Posn
    ; returns Last Posn of the shape
    get-pos
    ))

;;------------------------------------------------------------------------

; A Circle is a (new Circle% [x Coordinate][y Coordinate][r NonNegReal]
;                            [new-c Boolean][ev ShapeEvent][pos-from Posn])
; Represents a circle
; INTERP: x and y represent the x and y coordiante of center of circle resp.
; r is the radius of the gien circle
; new-c represents if the circle is new and is being constructed
; ev is the ShapeEvent which stores current event on shape being performed
; pos-from stores the position where the circle was started to be dragged

(define Circle%
  (class* object% (Shape<%>)
    (init-field x y r new-c ev pos-from)
    
    ; get-top : -> PosInt
    ; Returns the top - y coordinate of the bounding box    
    (define (get-top)
      (- y r))
    
    
    ; get-left : -> PosInt
    ; Returns the left - x coordinate of the bounding box    
    (define (get-left)
      (- x r))
    
    ; get-right : -> PosInt
    ; Returns the right - x coordinate of the bounding box    
    (define (get-right)
      (+ x r))
    
    ; get-bottom : -> PosInt
    ; Returns the bottom - y coordinate of the bounding box    
    (define (get-bottom)
      (+ y r))
    
    ; get-new-s : -> Boolean
    ; EXAMPLES:
    (define/public (get-new-s)
      new-c)
    
    ; get-event : -> ShapeEvent
    (define/public (get-event)
      ev)
    
    ; get-pos : -> Posn
    (define/public (get-pos)
      pos-from)
    
    ; left-limit : NonNegReal -> NonNegReal
    ; Gets the left limit of the circle control region
    ; which is 2 pixels less than the radius of the circle
    (define (left-limit r)
      (- r CIRCLE-BOUND))
    
    
    ; right-limit : NonNegReal -> NonNegReal
    ; Gets the right limit of the circle control region
    ; which is 2 pixels more than the radius of the circle
    (define (right-limit r)
      (+ r CIRCLE-BOUND))
    
    
    ; value-to-check : Coordinate Coordinate -> PosReal
    ; Gets the value that needs to be checked if it is between
    ; the control region endpoints
    ; EXAMPLES:
    ; GIVEN : mx = 0, my = 0 , x = 1 , y= 0
    ; RETURNS : 1
    (define (value-to-check mx my)
      (get-dist x mx y my))
    
    
    ; control-region? : Coordinate Coordinate -> Boolean
    ; Gets the value that needs to be checked if it is between
    ; the control region endpoints
    ; EXAMPLES:
    ; GIVEN : mx = 10 , my = 10 , rad = 5 ,x =6 , y= 10
    ; RETURNS : true
    (define (control-region? mx my)
      (<= (left-limit r) (value-to-check mx my) (right-limit r)))
    
    
    ; on-shape? : Coordinate Coordinate -> Boolean
    ; Checks if the mouse click is on the shape or not
    ; EXAMPLES:
    ; GIVEN : mx = 7 , my = 10 , rad = 5 ,x =6 , y= 10
    ; RETURNS : true
    (define (on-shape? mx my)
      (< (get-dist x mx y my) (- r CIRCLE-BOUND)))
    
    
    ; get-moved-coord : Coordinate Symbol -> Coordinate
    ; Gets the moved x or y coordinate when the shape is dragged 
    ; mc = moved coordinate and coord is the axis
    ; EXAMPLES:
    ; GIVEN : mc = 10 , coord = 'x , x = 5 , y= 5, posn-x of pos-from = 3
    ; RETURNS : 12
    ; STRATEGY : Data decomposition on coord : CircleCoords
    (define (get-moved-coord mc coord)
      (cond
           [(symbol=? coord X) (+ x (- mc (posn-x pos-from)))]
           [(symbol=? coord Y) (+ y (- mc (posn-y pos-from)))]))
    
    
    
    ; get-new-circle : Coordinate Coordinate NonNegReal
    ;                      Boolean ShapeEvent Posn -> Circle<%>
    ; Gets a new circle with given parameters as fields
    (define (get-new-circle xc yc rad nc eve ps-frm)
      (new Circle% [x xc] [y yc] 
           [r rad] 
           [new-c nc] [ev eve]
           [pos-from ps-frm]))
    
    
    ; btn-down : Coordinate Coordinate -> Circle<%>
    ; Checks if the button click is in control region, on the circle or 
    ; anywhere else and gets the edited circle according to that
    ; STRATEGY: Function Composition
    (define (btn-down mx my)
      (cond
        [(control-region? mx my) (get-new-circle x y (get-dist x mx y my)
                                                 false RESIZE pos-from)]
        [(on-shape? mx my) (get-new-circle x y r false MOVE (make-posn mx my))]
        [else (get-new-circle x y r false NONE pos-from)]))
    
    
    ; btn-drag : Coordinate Coordinate -> Circle<%>
    ; Checks what kind of event is being performed on the circle and
    ; gets the new shape according to that in case of drag
    ; STRATEGY : Data decomposition on ev : ShapeEvent
    (define (btn-drag mx my)
      (cond
          [(resize? ev) (get-new-circle x y (get-dist x mx y my)
                                                (if new-c true false)
                                                RESIZE pos-from)]
          [(move? ev) (get-new-circle (get-moved-coord mx X) 
                                           (get-moved-coord my 'y) 
                                           r false MOVE (make-posn mx my))] 
          [(none? ev ) (get-new-circle x y r new-c ev pos-from)]))
    
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the given shape which is the most
    ; fitting rectangle in which the shape fits perfectly
    (define/public (get-bounds)
      (list (get-left)
            (get-top)
            (get-right)
            (get-bottom)))    
    
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Circle<%>
    ; STRATEGY : Data decomposition on mev : MouseEvent
    (define/public (handle-mouse mx my mev)
      (cond
        [(string=? mev BTN-UP) 
         (get-new-circle x y r false NONE (make-posn ZERO ZERO))]
        [(string=? mev BTN-DOWN) (btn-down mx my)]
        [(string=? mev BTN-DRAG) (btn-drag mx my)]))
    
    ; render-shape : Image -> Image
    ; Takes in an image and a self rendering is placed on that image
    (define/public (render-shape scn)
      (local (
              ; shape-with-color : -> Image
              ; Returns the circle red if its being made else just a 
              ; black outline
              (define shape-with-color
                (if  new-c
                    (circle r OPACITY OP-COL)
                    (circle r OUTLINE TXT-COL-NORM))))
        (place-image shape-with-color x y scn)))
    
    (super-new)))

;;------------------------------------------------------------------------

; A Rectangle is a (new Rectangle% [l Coordinate][t Coordinate]
;                                  [r Coordinate][b Coordinate]
;                                  [new-r Boolean][ev ShapeEvent]
;                                  [pos-from Posn])
; Represents a rectangle
; INTERP: l represents min x coordinate of rectangle
; t represents the min y coordiante of the rectangle
; r represents the max x coordinate of the rectangle
; b represents the max y coordinate of the rectangle
; new-r represents if the rectangle is new and is being constructed
; ev is the ShapeEvent which stores current event on shape being performed
; pos-from stores the position where the rectangle was started to be dragged
; It is also used to store the pivot point if the rectangle is resized
; INVARIANTS : where l is less than equal to r and t is less than equal to b
(define Rectangle%
  (class* object% (Shape<%>)
    (init-field l t r b new-r ev pos-from)
    
    ; get-top : -> PosInt
    ; Returns the top - y coordinate of the bounding box    
    (define (get-top)
      t)
    
    
    ; get-left : -> PosInt
    ; Returns the left - x coordinate of the bounding box    
    (define (get-left)
      l)
    
    ; get-right : -> PosInt
    ; Returns the right - x coordinate of the bounding box    
    (define (get-right)
      r)
    
    ; get-bottom : -> PosInt
    ; Returns the bottom - y coordinate of the bounding box    
    (define (get-bottom)
      b)
    
    ; get-new-s : -> Boolean
    (define/public (get-new-s)
      new-r)
    
     ; get-event : -> ShapeEvent
    (define/public (get-event)
      ev)
    
    ; get-pos : -> Posn
    (define/public (get-pos)
      pos-from)
    
    ; control-region? : Coordinate Coordinate -> Boolean
    ; Gets the value that needs to be checked if it is between
    ; the control region endpoints
    (define (control-region? mx my)
      (or (resizing-from-left-top? mx my)
          (resizing-from-right-top? mx my)
          (resizing-from-left-bottom? mx my)
          (resizing-from-right-bottom? mx my)))
    
    ; on-shape? : Coordinate Coordinate -> Boolean
    ; Checks if the mouse click is on the shape or not    
    (define (on-shape? mx my)
      (and (<= l mx r)
           (<= t my b)))
    
    ; get-moved-coord : Coordinate Symbol -> Coordinate
    ; Gets the moved x or y coordinate when the shape is dragged    
    ; STRATEGY : Data decomposition on coord : RectangleCoords
    (define (get-moved-coord mc coord)
      (cond
           [(symbol=? coord L) (+ l (- mc (posn-x pos-from)))]
           [(symbol=? coord T) (+ t (- mc (posn-y pos-from)))]
           [(symbol=? coord R) (+ r (- mc (posn-x pos-from)))]
           [(symbol=? coord B) (+ b (- mc (posn-y pos-from)))]))
    
    
    ; left-limit : NonNegReal -> NonNegReal
    ; Gets the left limit of control region of a vertex
    ; which is 5 pixels less than the vertex coordinate
    (define (left-limit v)
      (- v RECTANGLE-BOUND))
    
    
    ; right-limit : NonNegReal -> NonNegReal
    ; Gets the right limit of control region of a vertex
    ; which is 5 pixels more than the vertex coordinate
    (define (right-limit v)
      (+ v RECTANGLE-BOUND))
    
    
    ; value-to-check : Coordinate -> Coordinate
    ; Returns the current coordiante as this has to be
    ; checked if it is existed in the bounds
    (define (value-to-check vc)
      vc)
    
    
    ; resizing-from-left-top? : Coordinate Coordinate -> Boolean
    ; Checks if the rectangle is being resized from the top left vertex
    (define (resizing-from-left-top? mx my)
      (and (<= (left-limit l) (value-to-check mx) (right-limit l))
           (<= (left-limit t) (value-to-check my) (right-limit t))))
    
    ; resizing-from-right-top? : Coordinate Coordinate -> Boolean
    ; Checks if the rectangle is being resized from the top right vertex
    (define (resizing-from-right-top? mx my)
      (and (<= (left-limit r) (value-to-check mx) (right-limit r))
           (<= (left-limit t) (value-to-check my) (right-limit t))))
    
    ; resizing-from-left-bottom? : Coordinate Coordinate -> Boolean
    ; Checks if the rectangle is being resized from the left bottom vertex
    (define (resizing-from-left-bottom? mx my)
      (and (<= (left-limit l) (value-to-check mx) (right-limit l))
           (<= (left-limit b) (value-to-check my) (right-limit b))))
    
    
    ; resizing-from-right-bottom? : Coordinate Coordinate -> Boolean
    ; Checks if the rectangle is being resized from the right bottom vertex
    (define (resizing-from-right-bottom? mx my)
      (and (<= (left-limit r) (value-to-check mx) (right-limit r))
           (<= (left-limit b) (value-to-check my) (right-limit b))))
    
    
    ; get-new-rectangle : Coordinate Coordinate Coordinate Coordiante
    ;                      Boolean ShapeEvent Posn -> Rectangle<%>
    ; Gets a new rectangle with given parameters as fields
    (define (get-new-rectangle left top right bottom nr eve ps-frm)
      (new Rectangle% [l left] [t top] [r right] [b bottom] 
           [new-r nr] [ev eve] [pos-from ps-frm])) 
    
    
    ; set-pos-from : Coordinate Coordinate -> Rectangle<%>
    ; Sets the value of the pos-from field and returns a 
    ; new rectangle
    (define/public (set-pos-from mx my)
      (get-new-rectangle l t r b new-r ev (make-posn mx my)))      
    
    
    ; get-resized-left : Coordinate Coordinate -> Coordinate
    ; Gets the new left coordinate of the rectangle after resizing
    ; computes the minimum of pivot x and mouse click x    
    ; STRATEGY : Data decomposition on pos-from : Posn
    (define (get-resized-left mx my)
      (min mx (posn-x pos-from)))
    
    ; get-resized-right : Coordinate Coordinate -> Coordinate
    ; Gets the new right coordinate of the rectangle after resizing
    ; computes the maximum of pivot x and mouse click x    
    ; STRATEGY : Data decomposition on pos-from : Posn
    (define (get-resized-right mx my)
      (max mx (posn-x pos-from)))
    
    
    ; get-resized-top : Coordinate Coordinate -> Coordinate
    ; Gets the new top coordinate of the rectangle after resizing
    ; computes the minimum of pivot y and mouse click y    
    ; STRATEGY : Data decomposition on pos-from : Posn
    (define (get-resized-top mx my)
      (min my (posn-y pos-from)))
    
    
    ; get-resized-bottom : Coordinate Coordinate -> Coordinate
    ; Gets the new bottom coordinate of the rectangle after resizing
    ; computes the minimum of pivot y and mouse click y    
    ; STRATEGY : Data decomposition on pos-from : Posn
    (define (get-resized-bottom mx my)
      (max my (posn-y pos-from)))
    
    
    ; snap-left : Coordinate Coordinate -> Coordinate
    ; Gets the new left coordinate of the rectangle when resizing
    ; has just begun, it snaps to the current mouse click x if the
    ; control region is the left top or left bottom
    (define (snap-left mx my)
      (if (or (resizing-from-left-top? mx my)
              (resizing-from-left-bottom? mx my))
          mx
          l))
    
    ; snap-top : Coordinate Coordinate -> Coordinate
    ; Gets the new top coordinate of the rectangle when resizing
    ; has just begun, it snaps to the current mouse click y if the
    ; control region is the left top or right top
    (define (snap-top mx my)
      (if (or (resizing-from-right-top? mx my)
             (resizing-from-left-top? mx my))
          my
          t))
    
    
    ; snap-right : Coordinate Coordinate -> Coordinate
    ; Gets the new right coordinate of the rectangle when resizing
    ; has just begun, it snaps to the current mouse click x if the
    ; control region is the right bottom or right top
    (define (snap-right mx my)
      (if (or (resizing-from-right-top? mx my)
             (resizing-from-right-bottom? mx my))
          mx
          r))
    
    
    ; snap-bottom : Coordinate Coordinate -> Coordinate
    ; Gets the new bottom coordinate of the rectangle when resizing
    ; has just begun, it snaps to the current mouse click y if the
    ; control region is the left bottom or right bottom
    (define (snap-bottom mx my)
      (if (or (resizing-from-left-bottom? mx my)
             (resizing-from-right-bottom? mx my))
          my
          b))
    
    
    ; get-pivot-point : Coordinate Coordinate -> Posn
    ; Gets the pivot point posn which is basically the diagonally
    ; opposite point of the point where the control region is
    ; currently while resizing
    ; STRATEGY: Function Composition
    (define (get-pivot-point mx my)
      (cond
           [(resizing-from-left-top? mx my)
                (make-posn r b)]
           [(resizing-from-right-top? mx my)
                (make-posn l b)]
           [(resizing-from-left-bottom? mx my)
                (make-posn r t)]
           [(resizing-from-right-bottom? mx my)
                (make-posn l t)]))
    
    
    ; btn-down : Coordinate Coordinate -> Rectangle<%>
    ; Checks if the button click is in control region, on the rectangle or 
    ; anywhere else and gets the edited rectangle according to that
    ; STRATEGY: Function Composition 
    (define (btn-down mx my)
      (cond
        [(control-region? mx my) 
         (get-new-rectangle (snap-left mx my)
                            (snap-top mx my)
                            (snap-right mx my)
                            (snap-bottom mx my)
                            false RESIZE
                            (get-pivot-point mx my))]
        
        [(on-shape? mx my) 
         (get-new-rectangle l t r b false MOVE (make-posn mx my))]
           [else (get-new-rectangle l t r b false NONE pos-from)]))
      
    
    
    ; btn-drag : Coordinate Coordinate -> Rectangle<%>
    ; Checks what kind of event is being performed on the 
    ; rectangle and gets the new shape according to that in
    ; case of drag
    ; STRATEGY : Data decomposition on ev : ShapeEvent
    (define (btn-drag mx my)
      (cond
          [(resize? ev) (get-new-rectangle (get-resized-left mx my)
                                               (get-resized-top mx my)
                                               (get-resized-right mx my)
                                               (get-resized-bottom mx my)
                                               (if new-r true false)
                                               RESIZE pos-from)]
          
          [(move? ev) (get-new-rectangle (get-moved-coord mx L)
                                            (get-moved-coord my T)
                                            (get-moved-coord mx R)
                                            (get-moved-coord my B)
                                            false MOVE (make-posn mx my))]
           
          [(none? ev) (get-new-rectangle l t r b new-r ev pos-from)]))
    
    
    
    ; get-bounds : -> BoundingBox
    ; Returns the bounding box of the given shape which is the most
    ; fitting rectangle in which the shape fits perfectly
    (define/public (get-bounds)
      (list (get-left)
            (get-top)
            (get-right)
            (get-bottom)))
    
    ; handle-mouse : Coordinate Coordinate MouseEvent -> Rectangle<%>
    ; STRATEGY : Data decomposition on mev : MouseEvent
    (define/public (handle-mouse mx my mev)
      (cond
        [(string=? mev BTN-UP) 
         (new Rectangle% 
              [l l]
              [t t]
              [r r]
              [b b]
              [new-r false]
              [ev NONE]
              [pos-from pos-from])]
        [(string=? mev BTN-DOWN) (btn-down mx my)]
        [(string=? mev BTN-DRAG) (btn-drag mx my)]))
    
    
    ; render-shape : Image -> Image
    ; Takes in an image and a self rendering is placed on that image
    (define/public (render-shape scn)
      (local (
              ; shape-with-color : -> Image
              ; Returns the rectange red if its being made else just a 
              ; black outline
              (define shape-with-color
                (if  new-r
                    (rectangle (abs-diff r l) 
                               (abs-diff b t) 
                               OPACITY OP-COL)
                    (rectangle (abs-diff r l) 
                               (abs-diff b t) 
                               OUTLINE TXT-COL-NORM))))
        (place-image shape-with-color (get-mid-point l r)
                   (get-mid-point t b) scn)))
    
    (super-new)))

;; Functions 
;;----------------------------------------------------------------------

; get-world-shapes : World -> ListOf<Shape<%>>
; GIVEN: A World,
; RETURNS: All the Shape<%>s which make up that world, i.e. all those that
;    have been created by the user through using the tools

; Examples:
; GIVEN : INITIAL-WORLD ; RETURNS empty
; Strategy : Function Composition
(define (get-world-shapes w)
  (send w get-shapes))
 
;;----------------------------------------------------------------------

; create-circle : posn Integer -> Shape<%>
; GIVEN: A center point and a radius
; RETURNS: A new Circle% object (implementing Shape<%>) with its center at
; the given point and radius as given.
; Strategy: Data decomposition on pos : Posn
(define (create-circle pos rad)
  (new Circle% 
       [x (posn-x pos)]
       [y (posn-y pos)]
       [r rad]
       [new-c false]
       [ev NONE]
       [pos-from (make-posn ZERO ZERO)]))

 ;;----------------------------------------------------------------------

; create-rectangle : BoundingBox -> Shape<%>
; GIVEN: A bounding box,
; RETURNS: A new Rectangle% object (implementing Shape<%>) which is bounded
; by the given BoundingBox.
; Strategy: Data decomposition on bounding-box : BoundingBox
(define (create-rectangle bounding-box)
  (new Rectangle% [l (first bounding-box)]
                [t (second bounding-box)]
                [r (third bounding-box)]
                [b (fourth bounding-box)]
                [new-r false]
                [ev NONE]
                [pos-from (make-posn ZERO ZERO)]))

;;----------------------------------------------------------------------

; create-circle-new : posn Integer -> Shape<%>
; GIVEN: A center point and a radius
; RETURNS: A new Circle% object (implementing Shape<%>) with its center at
; the given point and radius as given, with new-c = true
; Strategy: Data decomposition on pos : Posn
(define (create-circle-new pos rad)
  (new Circle% 
       [x (posn-x pos)]
       [y (posn-y pos)]
       [r rad]
       [new-c true]
       [ev RESIZE]
       [pos-from (make-posn ZERO ZERO)]))

 ;;----------------------------------------------------------------------

; create-rectangle-new : BoundingBox -> Shape<%>
; GIVEN: A bounding box,
; RETURNS: A new Rectangle% object (implementing Shape<%>) which is bounded
; by the given BoundingBox, with new-r = true
; Strategy: Data decomposition on bounding-box : BoundingBox
(define (create-rectangle-new bounding-box)
  (new Rectangle% [l (first bounding-box)]
                [t (second bounding-box)]
                [r (third bounding-box)]
                [b (fourth bounding-box)]
                [new-r true]
                [ev RESIZE]
                [pos-from (make-posn ZERO ZERO)]))

;;----------------------------------------------------------------------

;; get-dist : Coordinate Coordinate Coordinate Coordinate -> PosInt
;; Will return the distance between two points x1 y1 , x2 y2
;; EXAMPLES:
(begin-for-test
  (check-equal? (get-dist 10 11 10 10)
                1
                "distance is 1"))
;; Strategy: Function Composition
(define (get-dist x1 x2 y1 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

;;----------------------------------------------------------------------

;; get-mid-point : Coordinate Coordinate -> Real
;; Wll return the mid point of the given two coordinates
;; EXAMPLES:
(begin-for-test
  (check-equal? (get-mid-point  10 30)
                20
                "mid-point"))
;; STRATEGY : Function Composition
(define (get-mid-point c1 c2)
  (/ (+ c1 c2) MD-PNT-DIV))

;;----------------------------------------------------------------------

;; abs-diff : Coordinate Coordinate -> PosReal
;; Gets the absolute difference of two coordiantes
;; EXAMPLES:
(begin-for-test
  (check-equal? (abs-diff  -50 -30)
                20
                "aabsolute difference"))
;; STRATEGY : Function Composition
(define (abs-diff c1 c2)
  (abs (- c1 c2)))

;;----------------------------------------------------------------------

;; run : World% -> World<%>
;; Runs the big bang for the world
;; STRATEGY : Function Composition
(define (run w)
  (big-bang w
            (on-tick next-world)
            (on-mouse handle-mouse)
            (on-draw draw-world)))

;;----------------------------------------------------------------------

;; next-world : World% -> World%
;; Gets the next state of the world
;; STRATEGY : Function Composition
(define (next-world w)
  (send w next-world))

;;----------------------------------------------------------------------

;; draw-world : World% -> Image
;; Gets the rendering of the world to draw
;; STRATEGY : Function Composition
(define (draw-world w)
  (send w render))

;;----------------------------------------------------------------------

; handle-mouse : World% Coordinate Coordinate MouseEvent -> World%
; Handles the mouse event that the user preforms and 
; updates the state of the world on the basis of the kind
; of the event and where the cursor is clicked.
; Strategy : Function Composition
(define (handle-mouse w mx my mev)
  (send w mouse-handler mx my mev))

;;----------------------------------------------------------------------

; shape-equal? : Shape% Shape% -> Boolean
; Compares two Shape and returns True if each component of the given shapes
; are equal , this function is for testing purpose only
; Strategy: Function Composition
(define (shape-equal? s1 s2)
  (and
   (equal?
    (send s1 get-bounds)
    (send s2 get-bounds))
   (eq?
    (send s1 get-new-s)
    (send s2 get-new-s))
   (eq?
    (send s1 get-event)
    (send s2 get-event))
   (equal?
    (send s1 get-pos)
    (send s2 get-pos))))

;;----------------------------------------------------------------------

; shapes-equal-aux? : ListOf<Shape<%>> ListOf<Shape<%>> -> LisOf<Boolean>
; Compares two ListOf<Shape<%>> and returns list of True if each component 
; of the given shapes are equal ,this function is for testing purpose only
; Strategy: Data decomposition on ls1, ls2 : ListOf<Shapes<%>>
(define (shapes-equal-aux? ls1 ls2)
  (cond
       [(empty? ls1) (list #t)]
       [else (cons (shape-equal? (first ls1) (first ls2))
                   (shapes-equal-aux? (rest ls1) (rest ls2)))]))
;;----------------------------------------------------------------------

; shapes-equal? : ListOf<Shape<%>> ListOf<Shape<%>> -> Boolean
; Compares two ListOf<Shape<%>> and returns True if each component 
; of the given shapes are equal, this function is for testing purpose only
; Strategy: Function Composition 

(define (shapes-equal? ls1 ls2)
  (and (= (length ls1) (length ls2))
       (ormap (λ (x) (boolean=? true x)) (shapes-equal-aux? ls1 ls2))))

;;----------------------------------------------------------------------

; world-equal? : World% World% -> Boolean
; Compares two World and returns True if each component of the given world
; are equal , this function is for testing purpose only
; Strategy: Function Composition
(define (world-equal? w1 w2)
  (and
   (shapes-equal? 
    (send w1 get-shapes)  (send w2 get-shapes))
   (eq? (send w1 get-selection ) (send w2 get-selection ))
   (eq? (send w1 get-mouse-ev ) (send w2 get-mouse-ev ))
   (equal? (send w1 get-start-posn ) (send w2 get-start-posn ))
   (equal? (send w1 get-current-posn ) (send w2 get-current-posn ))))


;;------------------------------------------------------------------------

;; Testing Constants

(define TEST-RECT-1 (create-rectangle (list 100 100 150 150)))
(define TEST-RECT-2 (create-rectangle (list 200 200 350 350)))
(define TEST-CIRCLE-1 (create-circle (make-posn 100 100) 50))
(define TEST-CIRCLE-2 (create-circle (make-posn 80 80) 30))

;; INITIAL-WORLD

(define INITIAL-WORLD 
  (new World% [shapes empty] 
       [selection POINTER-SEL]
       [ex-mouse-ev ""]
       [start-posn (make-posn 0 0)]
       [current-posn (make-posn 0 0)]))

(define INITIAL-WORLD-2
  (new World% [shapes empty] 
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-UP]
       [start-posn (make-posn 0 0)]
       [current-posn (make-posn 0 0)]))


;; Circle
(define NEW-CIR
  (new Circle% [x 20]
       [y 20]
       [r 40]
       [new-c true]
       [ev MOVE]
       [pos-from (make-posn 0 0)]))

(define TEST-CIRCLE-1-SNAP 
  (new Circle% [x 100]
       [y 100]
       [r 51]
       [new-c false]
       [ev RESIZE]
       [pos-from (make-posn 0 0)]))


(define TEST-CIRCLE-1-RESIZED 
  (new Circle%  [x 100]
       [y 100]
       [r 55]
       [new-c false]
       [ev RESIZE]
       [pos-from (make-posn 0 0)]))


(define TEST-CIRCLE-1-SNAP-RED 
  (new Circle% [x 100]
       [y 100]
       [r 51]
       [new-c true]
       [ev RESIZE]
       [pos-from (make-posn 0 0)]))


(define TEST-CIRCLE-1-RESIZED-RED 
  (new Circle% [x 100]
       [y 100]
       [r 55]
       [new-c true]
       [ev RESIZE]
       [pos-from (make-posn 0 0)]))

(define TEST-CIRCLE-1-MOVE 
  (new Circle% [x 100]
       [y 100]
       [r 50]
       [new-c false]
       [ev MOVE]
       [pos-from (make-posn 120 120)]))

(define TEST-CIRCLE-1-MOVED 
  (new Circle% [x 110]
       [y 110]
       [r 50]
       [new-c false]
       [ev MOVE]
       [pos-from (make-posn 130 130)]))


(define TEST-RECT-NEW 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 300]
       [b 300]
       [new-r true]
       [ev NONE]
       [pos-from (make-posn ZERO ZERO)]))

(define TEST-RECT-SNAP-TR 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 151]
       [b 150]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 100 150)]))

(define TEST-RECT-SNAP-LT
  (new Rectangle% 
       [l 99]
       [t 100]
       [r 150]
       [b 150]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 150 150)]))

(define TEST-RECT-SNAP-BR 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 150]
       [b 149]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 100 100)]))


(define TEST-RECT-SNAP-LB
  (new Rectangle% 
       [l 101]
       [t 100]
       [r 150]
       [b 150]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 150 100)]))

(define TEST-RECT-RESIZED-TR 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 155]
       [b 150]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 100 150)]))
(define TEST-RECT-RESIZED-LT 
  (new Rectangle% 
       [l 50]
       [t 50]
       [r 150]
       [b 150]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 150 150)]))
(define TEST-RECT-RESIZED-BR 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 160]
       [b 160]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 100 100)]))
(define TEST-RECT-RESIZED-LB 
  (new Rectangle% 
       [l 150]
       [t 100]
       [r 155]
       [b 100]
       [new-r false]
       [ev RESIZE]
       [pos-from (make-posn 150 100)]))

(define TEST-RECT-MOVE
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 150]
       [b 150]
       [new-r false]
       [ev MOVE]
       [pos-from (make-posn 120 120)]))

(define TEST-RECT-MOVED
  (new Rectangle% 
       [l 110]
       [t 110]
       [r 160]
       [b 160]
       [new-r false]
       [ev MOVE]
       [pos-from (make-posn 130 130)]))
(define TEST-RECT-SNAP-BR-RED 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 150]
       [b 149]
       [new-r true]
       [ev RESIZE]
       [pos-from (make-posn 100 100)]))
(define TEST-RECT-RESIZED-BR-RED 
  (new Rectangle% 
       [l 100]
       [t 100]
       [r 160]
       [b 160]
       [new-r true]
       [ev RESIZE]
       [pos-from (make-posn 100 100)]))

;; LIST

(define CIRCLE-LS (list TEST-CIRCLE-1 TEST-CIRCLE-2))
(define RECT-LS (list TEST-RECT-1 TEST-RECT-2))  
(define SHP-LS (list TEST-CIRCLE-1 TEST-CIRCLE-2 TEST-RECT-1 TEST-RECT-2))  

;;WORLD
(define WORLD-TEST 
  (new World% [shapes SHP-LS]
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 0 0)] 
       [current-posn (make-posn 0 0)]))



(define WORLD-TEST1 
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 0 0)] 
       [current-posn (make-posn 0 0)]))

(define WORLD-TEST1-HOV 
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection POINTER-SEL]
       [ex-mouse-ev "hover" ]  
       [start-posn (make-posn 0 0)]
       [current-posn (make-posn 0 0)]))


(define WORLD-TEST1-REC 
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection RECTANGLE-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 15 30)]
       [current-posn (make-posn 15 30)]))

(define WORLD-TEST1-POINT
  (new World% [shapes (list TEST-CIRCLE-1)] 
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 15 10)]
       [current-posn (make-posn 15 10)]))

(define WORLD-TEST1-CIR
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection CIRCLE-SEL] 
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 15 50)]
       [current-posn (make-posn 15 50)]))

(define WORLD-TEST1-CIR-INV 
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection CIRCLE-SEL] 
       [ex-mouse-ev BTN-DRAG ]  
       [start-posn (make-posn 15 50)]
       [current-posn (make-posn 261 260)]))

(define WORLD-TEST1-CIR-ADD 
  (new World% [shapes (list (create-circle-new (make-posn 160 160 ) 0)
                            TEST-CIRCLE-1 )] 
       [selection CIRCLE-SEL] 
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 160 160)]
       [current-posn (make-posn 160 160)]))

(define WORLD-TEST1-REC-ADD
  (new World% 
       [shapes 
        (list 
         (send 
          (create-rectangle-new (list 160 160 160 160)) 
          set-pos-from 160 160) 
         TEST-CIRCLE-1 )]
       [selection RECTANGLE-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 160 160)] 
       [current-posn (make-posn 160 160)]))

(define WORLD-TEST1-CIR-DRAG 
  (new World% [shapes (list (create-circle-new (make-posn 160 160 ) 1) 
                            TEST-CIRCLE-1 )] 
       [selection CIRCLE-SEL]
       [ex-mouse-ev BTN-DRAG ]  
       [start-posn (make-posn 160 160)] 
       [current-posn (make-posn 161 160)]))

(define WORLD-TEST1-UP 
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-UP ]  
       [start-posn (make-posn 0 0)]
       [current-posn (make-posn 0 0)]))

(define WORLD-TEST1-MOVE
  (new World% [shapes (list TEST-CIRCLE-1-MOVE)]
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 120 120)]
       [current-posn (make-posn 120 120)]))

(define WORLD-TEST1-SNAP
  (new World% [shapes (list TEST-CIRCLE-1-SNAP)]
       [selection POINTER-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 120 120)]
       [current-posn (make-posn 120 120)]))

(define WORLD-TEST1-RESIZED 
  (new World% [shapes (list TEST-CIRCLE-1-RESIZED)]
       [selection POINTER-SEL] 
       [ex-mouse-ev BTN-DRAG ]  
       [start-posn (make-posn 120 120)]
       [current-posn (make-posn 155 100)]))

(define WORLD-TEST1-DRAG 
  (new World% [shapes (list TEST-CIRCLE-1)]
       [selection CIRCLE-SEL] 
       [ex-mouse-ev BTN-UP ]  
       [start-posn (make-posn 261 260)] 
       [current-posn (make-posn 261 260)]))


(define WORLD-TEST1-CIRC-ADD
  (new World% 
       [shapes 
        (list (create-circle-new (make-posn 160 160 ) 0)
                            TEST-RECT-1)]
       [selection CIRCLE-SEL]
       [ex-mouse-ev BTN-DOWN ]  
       [start-posn (make-posn 160 160)] 
       [current-posn (make-posn 160 160)]))




;; Testing
;; ---------------------------------------------------------------------------

;; Circle
(begin-for-test
  (check-false (send TEST-CIRCLE-1 get-new-s)
               "shape already created")
  
  (check-true (send NEW-CIR get-new-s)
              "shape getting created")
  
  (check-equal? (send TEST-CIRCLE-1 get-bounds)
                '(50 50 150 150) "limits of circle")
  
  (check-false (shape-equal? TEST-CIRCLE-1 TEST-CIRCLE-2)
               "not equal")
  
  (check-true (shape-equal? (send TEST-CIRCLE-1 handle-mouse 151 100 BTN-DOWN)
                            TEST-CIRCLE-1-SNAP)
              "shapes are equal")
  
  (check-true (shape-equal? (send TEST-CIRCLE-1 handle-mouse 120 120 BTN-DOWN)
                            TEST-CIRCLE-1-MOVE)
              "circle is moved")
  
  (check-true (shape-equal? (send TEST-CIRCLE-1 handle-mouse 160 160 BTN-DOWN)
                            TEST-CIRCLE-1)
              "not within control area ,world do not change")
  
  (check-true (shape-equal? (send TEST-CIRCLE-1 handle-mouse 160 160 BTN-UP)
                            TEST-CIRCLE-1)
              "not within control area ,world do not change")
  
  (check-true (shape-equal?
               (send TEST-CIRCLE-1-SNAP handle-mouse 155 100 BTN-DRAG)
               TEST-CIRCLE-1-RESIZED)
              "circle is resized")
  
  (check-true (shape-equal?
               (send TEST-CIRCLE-1-SNAP-RED handle-mouse 155 100 BTN-DRAG)
               TEST-CIRCLE-1-RESIZED-RED)
              "circle that is newly created is resized")
  
  (check-true (shape-equal?
               (send TEST-CIRCLE-1-MOVE handle-mouse 130 130 BTN-DRAG)
               TEST-CIRCLE-1-MOVED)
              "circle moved to new location")
  
  (check-true (shape-equal? (send TEST-CIRCLE-1 handle-mouse 160 160 BTN-DRAG)
                            TEST-CIRCLE-1)
              "not within control area world do not change"))


;; Test for rectangle
(begin-for-test
  (check-false (send TEST-RECT-1 get-new-s)
               "shape already created")
  
  (check-true (send TEST-RECT-NEW get-new-s)
              "new shaped")
  
  (check-equal? (send TEST-RECT-1 get-bounds)
                '(100 100 150 150) 
                "boundaries of rectangle")
  
  (check-false (shape-equal? TEST-RECT-1 TEST-RECT-2)
               "shapes are not equal ")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 151 100 BTN-DOWN)
                            TEST-RECT-SNAP-TR)
              "rectangle is snapped towards Top Right ")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 150 149 BTN-DOWN)
                            TEST-RECT-SNAP-BR)
              "rectangle is snapped towards Bottom Right")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 99 100 BTN-DOWN)
                            TEST-RECT-SNAP-LT)
              "rectangle is snapped towards left top ")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 101 150 BTN-DOWN)
                            TEST-RECT-SNAP-LB)
              "rectangle is snapped towards Left Bottom")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 20 20 BTN-DOWN)
                            TEST-RECT-1)
              "out of control ara, world do not change")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 20 20 BTN-UP)
                            TEST-RECT-1)
              "Out of control area , World do not change")
  
  (check-true (shape-equal? 
               (send TEST-RECT-SNAP-TR handle-mouse 155 100 BTN-DRAG)
               TEST-RECT-RESIZED-TR)
              "Recangle resized by top right corner")
  
  (check-true (shape-equal? (send TEST-RECT-NEW handle-mouse 155 100 BTN-DRAG)
                            TEST-RECT-NEW)
              "New rectangle is resized")
  
  (check-true (shape-equal? (send TEST-RECT-SNAP-LT handle-mouse 50 50 BTN-DRAG)
                            TEST-RECT-RESIZED-LT)
              "Recangle resized by top left corner")
  
  (check-true (shape-equal? 
               (send TEST-RECT-SNAP-BR handle-mouse 160 160 BTN-DRAG)
               TEST-RECT-RESIZED-BR)
              "Recangle resized by bottom right corner")
  
  (check-true (shape-equal? 
               (send TEST-RECT-SNAP-BR-RED handle-mouse 160 160 BTN-DRAG)
               TEST-RECT-RESIZED-BR-RED)
              "New rectangle is resized")
  
  (check-true (shape-equal?
               (send TEST-RECT-SNAP-LB handle-mouse 155 100 BTN-DRAG)
               TEST-RECT-RESIZED-LB)
              "Recangle resized by bottom left corner")
  
  (check-true (shape-equal? (send TEST-RECT-1 handle-mouse 120 120 BTN-DOWN)
                            TEST-RECT-MOVE)
              "Rectangle selected to be moved or resized")
  (check-true (shape-equal? (send TEST-RECT-MOVE handle-mouse 130 130 BTN-DRAG)
                            TEST-RECT-MOVED)
              "Rectangle moved"))


;; World Test Case

(begin-for-test
  (check-true (world-equal? (send WORLD-TEST next-world) 
                            WORLD-TEST)
              "world itself")
  (check-true (world-equal? (send WORLD-TEST1 mouse-handler 120 120 BTN-DOWN)
                            WORLD-TEST1-MOVE )
              "world is about to move")
  (check-true (world-equal? (send WORLD-TEST1-HOV mouse-handler 120 120 BTN-UP)
                            WORLD-TEST1-HOV )
              "do nothing on hover")
  (check-true (world-equal? 
               (send WORLD-TEST1-CIR-INV mouse-handler 261 260 BTN-UP)
               WORLD-TEST1-DRAG )
              " no change in world ")
  (check-true (world-equal? 
               (send WORLD-TEST1-SNAP mouse-handler 155 100 BTN-DRAG)
               WORLD-TEST1-RESIZED )
              "resiziing of shape in world ")
  (check-true (world-equal? (send WORLD-TEST1 mouse-handler 160 160 BTN-UP)
                            WORLD-TEST1-UP )
              " no change on button up ")
  (check-true (world-equal? 
               (send WORLD-TEST1 mouse-handler 160 160 "button-hover")
               WORLD-TEST1 )
              " no change on button-hover")
  (check-true (world-equal? (send WORLD-TEST1 mouse-handler 15 30 BTN-DOWN)
                            WORLD-TEST1-REC )
              "Toolbar selection rectangles")
  (check-true (world-equal? (send WORLD-TEST1-REC mouse-handler 15 10 BTN-DOWN)
                            WORLD-TEST1-POINT )
              "toolbar selection pointers")
  (check-true (world-equal? (send WORLD-TEST1 mouse-handler 15 50 BTN-DOWN)
                            WORLD-TEST1-CIR )
              "toolbar selection circle ")
  (check-true (world-equal? 
               (send WORLD-TEST1-CIR mouse-handler 160 160 BTN-DOWN)
               WORLD-TEST1-CIR-ADD )
              "adding new circle to the world")
  (check-true (world-equal? 
               (send WORLD-TEST1-CIR-ADD mouse-handler 161 160 BTN-DRAG)
               WORLD-TEST1-CIR-DRAG)
              "resizing new circle in the world")
  (check-true (world-equal? 
               (send WORLD-TEST1-CIR mouse-handler 261 260 BTN-DRAG)
               WORLD-TEST1-CIR-INV)
              "invalid drag")
  
  (check-true (world-equal? 
               (send WORLD-TEST1-REC mouse-handler 160 160 BTN-DOWN)
               WORLD-TEST1-REC-ADD )
              "addinf rectanngle to the world"))


(begin-for-test
  (check-true (shape-equal? (send TEST-RECT-RESIZED-BR-RED set-pos-from 100 100)
                            TEST-RECT-RESIZED-BR-RED )
              "shapes are equal")) 



(begin-for-test
  (check-equal? (send WORLD-TEST1-REC-ADD render)
               (overlay/align LEFT TOP
                     (above (overlay (text "p" TEXT-SIZE TXT-COL-NORM)
             (rectangle TXT-BOX-WD TXT-BOX-HT OUTLINE TXT-COL-NORM))
                            (overlay (text "r" TEXT-SIZE TXT-COL-INV)
             (rectangle TXT-BOX-WD TXT-BOX-HT SOLID TXT-COL-NORM))
                            (overlay (text "c" TEXT-SIZE TXT-COL-NORM)
             (rectangle TXT-BOX-WD TXT-BOX-HT OUTLINE TXT-COL-NORM)))
                  (place-image (rectangle 0 0 OPACITY OP-COL) 
                               160 160 (place-image
                                        (circle 50 OUTLINE TXT-COL-NORM) 
                                                   100 100 EMPTY-SCENE)))
               "renders a new rectangle and circle with rect selected tool"))


(begin-for-test
  (check-equal? (draw-world WORLD-TEST1-CIRC-ADD)
               (overlay/align LEFT TOP
                     (above (overlay (text "p" TEXT-SIZE TXT-COL-NORM)
             (rectangle TXT-BOX-WD TXT-BOX-HT OUTLINE TXT-COL-NORM))
                            (overlay (text "r" TEXT-SIZE TXT-COL-NORM)
             (rectangle TXT-BOX-WD TXT-BOX-HT OUTLINE TXT-COL-NORM))
                            (overlay (text "c" TEXT-SIZE TXT-COL-INV)
             (rectangle TXT-BOX-WD TXT-BOX-HT SOLID TXT-COL-NORM)))
                  (place-image (circle 0 OPACITY OP-COL) 160 160 
                               (place-image 
                                (rectangle 50 50 OUTLINE TXT-COL-NORM) 
                                            125 125 EMPTY-SCENE)))
               "renders a new cirlce and an  rect with circle selected tool"))



(begin-for-test
  (check-equal? (get-world-shapes WORLD-TEST1-CIR) (list TEST-CIRCLE-1)
                "returns list of shapes"))

;next-world
(begin-for-test
  (check-true (world-equal? (next-world WORLD-TEST1) WORLD-TEST1)
              "returns World"))

(begin-for-test
  (check-true (world-equal? (handle-mouse INITIAL-WORLD-2 100 100 BTN-UP)
                            INITIAL-WORLD-2)
              "Worlds on mouseEvent UP"))
(run INITIAL-WORLD)

