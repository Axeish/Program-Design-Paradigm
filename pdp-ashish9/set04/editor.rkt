;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define  TIME-ON-TASK 15)

(check-location "04" "editor.rkt")

;Editor Constants
(define HEIGHT 20);posInteger
(define WIDTH 200);PosInteger
(define MTS (empty-scene WIDTH HEIGHT));Image
(define TEXTSIZE 15 );posInteger
(define TEXTCOLOR "black");Color
(define CURSOR (rectangle 1 20 "solid" "red"));image

;1String is a list of single length strings

;An Lo1S is one of:
; - empty 
; - (cons 1String Lo1S)

; INTERP: empty is a list with no elements 
; (cons 1String Lo1S) represents a list of single length string 
; where 1String is the first element and  Lo1s is the rest of the elements


; examples of  
(define GOOD 
  (cons "g" (cons "o" (cons "o"(cons "d" '())))))
(define DAY
  (cons "d"(cons "a"(cons"y" '()))))
(define GOODY 
  (cons "g" (cons "o" (cons "o"(cons "d" (cons "y" '()))))))


; TEMPLATE:
;(define (lo1s-fn l)
;  (cond 
;    [(empty? l)  '()]
;    [(cons? l)
;     (... (first l) ...  
;          (lo1s-fn (rest l )) ... ) ]))
 
; An editor is a (make-editor Lo1S Lo1S)
; INTERP: (make-editor l1 l2) has two list l1 and l2 which is imploded
; into each string and displayed with cursor between them
(define-struct editor [pre post])

; Editor Example
(define EDIT (make-editor GOOD DAY ) ) ;"abcdxyz"

; TEMPLATE
(define (editor-fn e)
  (... (lo1s-fn ( editor-pre e) ... (lo1s-fn (editor-post e)))))
  

;Stimulation Handler

;run : String -> World 
; will start stimulation 
; STRATEGY: function composition
(define (run str)
  (big-bang (string->editor str)
            (to-draw render)
            (on-key edit)))

; string->editor : String -> Editor
; Returns an Editor with empty list and Lo1S and cursor at position 0.
;WHERE :
;Lo1S is formed by exploding str
;Example:
(begin-for-test
  (check-equal? (string->editor "good")
                (make-editor '() GOOD)
                "new editor"))
;STRATEGY: Function Composition
(define (string->editor str)
  (make-editor '() (explode str)))

;  drawing functions
; render : Editor -> Image
; Will draw image by editor->image aligned on left and center of empty scene.
;Example
(begin-for-test 
  (check-equal? (render (make-editor GOOD DAY))  
         (overlay/align "left"  
                 "center"  
                 (beside (text "good" TEXTSIZE TEXTCOLOR)  
                     CURSOR  
                     (text "day" TEXTSIZE TEXTCOLOR))  
                 MTS)
         "Editor is drawn on empty screen"))
;STRATEGY: Function Composition
(define (render e)
  (overlay/align  "left" 
           "center"  
           (editor->image e)  
           MTS)) 


; editor->image : Editor -> Image
; Will take a editor and create iamge of text inside editor 
;Example:
(begin-for-test
  (check-equal? (editor->image (make-editor GOOD DAY))  
           
                 (beside (text "good" TEXTSIZE TEXTCOLOR)  
                     CURSOR  
                     (text "day" TEXTSIZE TEXTCOLOR))
 "image of a editor ") 
  )
;STRATEGY: Data Decomposition on e : Editor
(define (editor->image e)
  (beside (text (implode (editor-pre e)) TEXTSIZE TEXTCOLOR)  
               CURSOR  
               (text (implode (editor-post e)) TEXTSIZE TEXTCOLOR))) 





; key->image : key -> Image
; Will take a key and create its image
; where k is a key of a keyboard which is one one character length and not \t,
;del,\r
; Example:
(begin-for-test
  (check-equal? (key->image  "a")  
           (text "a" TEXTSIZE TEXTCOLOR) "image of a key ")) 
                     
;STRATEGY: function composition  
(define (key->image k)
   (text k TEXTSIZE TEXTCOLOR))


;on key function

; edit  : Editor KeyEvent -> Editor
; Call handler according to the key pressed
;Example:
(begin-for-test
 (check-equal? (edit EDIT "g") 
                (make-editor (list "g" "o" "o" "d" "g") (list "d" "a" "y"))
                "inserts g  to the left of cursor "))
;STRATEGY: data decomposition of key : KeyHandler
(define (edit e key)
  (cond
    [(key=? "\b" key) (delete-char e)]
    [(= (string-length key) 1) (type-char e key)]
    [(key=? "left" key) (move-cursor-left e)]
    [(key=? "right" key) (move-cursor-right e)]
    [else e]))
;Testing
(begin-for-test
 (check-equal? (edit EDIT "g") 
                (make-editor (list "g" "o" "o" "d" "g") (list "d" "a" "y"))
                "inserts g  to the left of cursor ")
 (check-equal? (edit EDIT "left") 
                (make-editor (list "g" "o" "o") (list "d" "d" "a" "y"))
                "move  cursor  left")
 (check-equal? (edit EDIT "right") 
                (make-editor (list "g" "o" "o" "d" "d") (list "a" "y"))
                "move cursor right ")
 (check-equal? (edit EDIT "\b") 
                (make-editor (list "g" "o" "o") (list "d" "a" "y"))
                "deletes from left ")
(check-equal? (edit EDIT "\t") 
                (make-editor (list "g" "o" "o" "d" ) (list "d" "a" "y"))
                "ignores ")
(check-equal? (edit EDIT "shift") 
                (make-editor (list "g" "o" "o" "d" ) (list "d" "a" "y"))
                "ignores "))



; type-char : Editor -> Editor
; Makes a new Editor
;Example:
(begin-for-test
 (check-equal? (type-char EDIT "g") 
                (make-editor (list "g" "o" "o" "d" "g") (list "d" "a" "y"))
                "inserts g  to the left of cursor "))
;STRATEGY: data decomposition of e :Editor
(define (type-char e key)
   (make-editor 
    (next-prefix e key) (editor-post e)))


;next-prefix: Editor Key -> Lo1S
; will take Editor and key and will return new prefix
;example
(begin-for-test
  (check-equal?  (next-prefix EDIT "k")
                (list "g" "o" "o" "d" "k")
                "new key added to end"))
;STRATEGY: data decomposition on e , Editor
(define (next-prefix e k )
  (if (or (to-be-ignored? k) (string-length-max? e k))
      (editor-pre e)
      (list-add-last (editor-pre e)  k )))
  
         
         
         
;to-be-ignored? : Key -> Boolean
; returns true if tab or delete key is pressed
; WHERE:
; key is any single character length key 
;Example:
  (begin-for-test
    (check-equal? (edit (make-editor '() GOOD) "\t")
                (make-editor '() GOOD)
                "ignores tab"))
  
;STRATEGY: function Composition  
(define (to-be-ignored? key )
  (or (string=? "\t" key) (string=? "\u007F" key) (string=? "\r" key)))


;string-length-max? : Editor Key  -> boolean
;Will return true if size of the string exceeds the editor area
;Example:
(begin-for-test 
  (check-equal? (string-length-max? EDIT "k")
                #false
                "string within limit"))

;STRATEGY: function Composition
(define (string-length-max? e k)
  (>= (image-width (beside
                    (editor->image e) (key->image k))) WIDTH))


; delete-char : Editor -> Editor
; Deletes a character that is just before the cursor
; Example:
(begin-for-test
  (check-equal? (edit (make-editor GOOD DAY) "\b") 
                (make-editor (reverse (rest (reverse GOOD))) DAY)
                "deletes character"))
;STRATEGY:
;data decomposition of e: Editor
(define (delete-char e)
  (if (empty? (editor-pre e)) e
  (make-editor (list-del-from-last (editor-pre e)) (editor-post e))))
;Testing
(begin-for-test
  (check-equal? (edit (make-editor '() DAY) "\b") 
                (make-editor '() DAY)
                "no action"))

; move-cursor-left : Editor -> Editor
; Move the cursor one character towards left
; Example:
(begin-for-test
  (check-equal? (move-cursor-left EDIT)
                (make-editor (list "g" "o" "o") (list  "d" "d" "a" "y"))
                "cursor moved to left"))
;STRATEGY:
;data decomposition of e: Editor  
(define (move-cursor-left e)
  (if (empty? (editor-pre e)) e 
      (make-editor (list-del-from-last (editor-pre e)) 
                   (cons (list-last (editor-pre e)) 
                                  (editor-post e)))))

;Testing
(begin-for-test
  (check-equal? (move-cursor-left (make-editor '() DAY )) 
                (make-editor '() DAY)
                "no action"))


; move-cursor-right : Editor -> Editor
; Move the cursor one character towards right
;example:
(begin-for-test
  (check-equal? (move-cursor-right EDIT)
                (make-editor (list "g" "o" "o" "d" "d") (list "a" "y"))
                "cursor moved to right"))


;STRATEGY: data decomposition of e: Editor  
(define (move-cursor-right e)
  (if (empty? (editor-post e)) e 
      (make-editor ( list-add-last (editor-pre e)  (first (editor-post e))) 
                   (rest (editor-post e)))))

;Testing
(begin-for-test
  (check-equal? (move-cursor-right (make-editor  DAY '())) 
                (make-editor  DAY '())
                "no action"))


;LIST functions
;---------------------------------------------------------------

;list-last : Lo1S -> 1String
;will return last element of the list
;WHERE: list is non empty
; example :
(begin-for-test
  (check-equal? (list-last GOOD)
                "d" "last element"))
;STRATEGY: Data decomposition on l, Lo1S
(define (list-last l)
  (first (reverse l)))


;list-del-from-last : Lo1S -> Lo1S
;will return a list of strings with last item removed
;WHERE: ist is non empty
(begin-for-test
  (check-equal? ( list-del-from-last GOOD)
                (cons "g" (cons "o" (cons "o" '() )))
                "list remove from last "))
;STRATEGY: Data decomposition on l, Lo1S
(define (list-del-from-last l )
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l) (list-del-from-last (rest l) ))]))
  

;list-add-last: Lo1S 1String -> Lo1s
;will add new 1String to end
;WHERE: list is non empty
;example
(begin-for-test
  (check-equal? (list-add-last GOOD "y")
                GOODY "added to last"))
;STRATEGY: Data decomposition on l, Lo1S
(define (list-add-last l s)
  (cond
    [(empty? l ) (cons s '())]
    [else (cons (first l) (list-add-last (rest l) s))]))


;Alternate Data Definition
;-------------------------------------------------------------------------

;1

;An editor is a (make-editor String String)
;INTERP:  (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t
;(define-struct editor [pre post])

;Editor Example
;(define EDIT (make-editor "abcd" "xyz" ) )

;TEMPLATE:
;(define (editor-fn e)
;  ( ... (editor-pre e) ... (editor-post e) ...))

;PROS-AS-COMPARED-TO-LIST
;- String is light weight data . list will have one constructor for every 
;character
;- Rendering is Easy 

;CONS-AS-COMPARED-TO-LIST
;- Adding and removing an element from list of 1String is easier
;when compared to string
;- Using recursion can add a lot of simplicity for every operation
;that requires change in size of pre and post



;2.

;Alternate Data Definition : Editor
;-------------------------------------------------------------------------
;1.
; Editor is (make-editor String Natural)  
; INTERP: the current text (txt) and cursor position (cp) 
;   
;
;;TEMPLATE:
;  (define (editor-fn e)  
;   (...  (editor-txt e) ... (editor-cp) ... )
;
;;example :
;(define ED (make-editor "asdfhgj" 3 ) ) - > asdf|hgj
;
;
;;PROS:
;-String Handling function that will become easy. we can use just
;string-insert and string-delete functions 

;;CONS:
;-rendering functions will need extra handlers to split the string, complex
; List is very easy when it comes to decomposing data .

;-----------------------------END----------------------------