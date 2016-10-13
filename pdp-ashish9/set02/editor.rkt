;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 12) 

(check-location "02" "editor.rkt")

(provide render)
(provide edit)
(provide string->editor)
(provide editor-pre)
(provide editor-post)
(provide editor-pos)

;Editor Constants
(define HEIGHT 20);posInteger
(define WIDTH 200);PosInteger
(define MTS (empty-scene WIDTH HEIGHT));Image
(define TEXTSIZE 15 );posInteger
(define TEXTCOLOR "black");Color
(define CURSOR (rectangle 1 20 "solid" "red"));image


;An editor is a (make-editor String String)
;INTERP:  (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t
(define-struct editor [pre post])

;Editor Example
(define EDIT (make-editor "abcd" "xyz" ) )

;TEMPLATE:
(define (editor-fn e)
  ( ... (editor-pre e) ... (editor-post e) ...))


; A KeyEvent is one of:
; - "a"
; - "b"
; - ...
; - "right"
; - "left"
; - ...
; INTERP: A KeyEvent represents a key-press in a Universe (big-bang) program.
; TEMPLATE:
; keyevent-fn : KeyEvent -> ???
;(define (keyevent-fn kev)
;  (cond
;    [(key=? kev "a") ...]
;    [(key=? kev "b") ...]
;    [(key=? kev "c") ...]
;    ...))

;Editor Functions
;-------------------------------------------------------------------------------

; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor e
;Example:
(begin-for-test
(check-equal? (editor-pos (make-editor "" "cd" )) 0 
              "position of cursor"))

;STRATEGY:
;data decomposition on e:Editor
(define (editor-pos e)
  (string-length (editor-pre e)))

(begin-for-test
(check-equal? (editor-pos (make-editor "" "abcd" )) 0
              "position of cursor")
(check-equal? (editor-pos (make-editor "abcd" "abcd" )) 4
              "position of cursor")
(check-equal? (editor-pos (make-editor "abcd" "" )) 4
              "position of cursor"))



; string->editor : String -> Editor
; Returns an Editor containing text str and cursor at position 0.
;WHERE :
;str is a string that will appear after the cursor
;Example:
(begin-for-test
  (check-equal? (string->editor "abcd") (make-editor "" "abcd" )
                "Editor is created")) 

;STRATEGY: function composition                
(define (string->editor str)
  (make-editor "" str))

(begin-for-test
  (check-equal? (string->editor "abcd") (make-editor "" "abcd" )
                "Editor is created with cursor followed by abcd")
  
  (check-equal? (string->editor "") (make-editor "" "" )
                "Editor is created without any string"))


; editor->image : Editor -> Image
; Will take a editor and create iamge of text inside editor 
;Example:
(begin-for-test
  (check-equal? (editor->image (make-editor "ashish" "kumar"))  
           
                 (beside (text "ashish" TEXTSIZE TEXTCOLOR)  
                     CURSOR  
                     (text "kumar" TEXTSIZE TEXTCOLOR))
 "image of a editor ") 
  )
;STRATEGY: Data Decomposition on e : Editor
(define (editor->image e)
  (beside (text (editor-pre e) TEXTSIZE TEXTCOLOR)  
               CURSOR  
               (text (editor-post e) TEXTSIZE TEXTCOLOR)))
(begin-for-test
  (check-equal? (editor->image (make-editor "ashish" "kumar"))  
           
                 (beside (text "ashish" TEXTSIZE TEXTCOLOR)  
                     CURSOR  
                     (text "kumar" TEXTSIZE TEXTCOLOR))
 "image of a editor ") 
  (check-equal? (editor->image (make-editor "" "kumar"))  
           
                 (beside CURSOR  
                     (text "kumar" TEXTSIZE TEXTCOLOR))
 "image of a editor with cursor at postion 0")
  (check-equal? (editor->image (make-editor "kumar" ""))  
           
                 (beside (text "kumar" TEXTSIZE TEXTCOLOR) CURSOR)
 "image of a editor with cursor at end of string"))


; key->image : key -> Image
; Will take a key and create its image
; where k is a key of a keyboard which is one one character length and not \t, del,\r
; Example:
(begin-for-test
  (check-equal? (key->image  "a")  
           (text "a" TEXTSIZE TEXTCOLOR) "image of a key ")) 
                     
;STRATEGY: function composition  
(define (key->image k)
   (text k TEXTSIZE TEXTCOLOR))

(begin-for-test
  (check-equal? (key->image  "a")  
           (text "a" TEXTSIZE TEXTCOLOR) "image of a key ")
  (check-equal? (key->image  " ")  
           (text " " TEXTSIZE TEXTCOLOR) "image of a space "))

; run : World-> World
; Will start stimulation
;STRATEGY: function compostion
(define (run str)
  (big-bang (string->editor str)
            (to-draw render)
            (on-key edit)))

; render : Editor -> Image
; Will draw image by editor->image aligned on left and center of empty scene.
;Example
(begin-for-test 
  (check-equal? (render (make-editor "ashish" "kumar"))  
         (overlay/align "left"  
                 "center"  
                 (beside (text "ashish" TEXTSIZE TEXTCOLOR)  
                     CURSOR  
                     (text "kumar" TEXTSIZE TEXTCOLOR))  
                 MTS)
         "Editor is drawn on empty screen"))

;STRATEGY: Function Composition
(define (render e)
  (overlay/align  "left" 
           "center"  
           (editor->image e)  
           MTS))  

(begin-for-test 
  (check-equal? (render (make-editor "" "kumar"))  
         (overlay/align "left"  
                 "center"  
                 (beside CURSOR  
                     (text "kumar" TEXTSIZE TEXTCOLOR))  
                 MTS)
         "editor is drawn on empty screen")
  (check-equal? (render (make-editor "ashish" "kumar"))  
         (overlay/align "left"  
                 "center"  
                 (beside (text "ashish" TEXTSIZE TEXTCOLOR)  
                     CURSOR  
                     (text "kumar" TEXTSIZE TEXTCOLOR))  
                 MTS)
         "editor is drawn on empty screen"))

; edit  : Editor KeyEvent -> Editor
; Call handler according to the key pressed
(begin-for-test
  (check-equal? (edit (make-editor "ashish" "kumar") "left")
                (make-editor "ashis" "hkumar")
                "cursor moves left")
  (check-equal? (edit (make-editor "ashish" "kumar") "\t")
                (make-editor "ashish" "kumar")
                "tab is ignored"))

;STRATEGY: data decomposition of e :Editor
(define (edit e key)
   (cond
    [(key=? "\b" key) (delete-char e)]
    [(= (string-length key) 1) (type-char e key)]
    [(key=? "left" key) (move-cursor-left e)]
    [(key=? "right" key) (move-cursor-right e)]
    [else e]))

(begin-for-test
  (check-equal? (edit (make-editor "aish" "kumar") "left")
                (make-editor "ais" "hkumar")
                "cursor moves left")
  (check-equal? (edit (make-editor "ash" "kumar") "\t")
                (make-editor "ash" "kumar")
                "tab is ignored")
  (check-equal? (edit (make-editor "ashish" "kumar") "right")
                (make-editor "ashishk" "umar")
                "cursors moves right")
  (check-equal? (edit (make-editor "ashish" "kumar") "\b")
                (make-editor "ashis" "kumar")
                "deletes left of cursor")
  (check-equal? (edit (make-editor "ashish" "kumar") "a") 
                (make-editor "ashisha" "kumar")
                "inserts a to the left of cursor")
  (check-equal? (edit (make-editor "ashish" "kumar") "shift") 
                (make-editor "ashish" "kumar")
                "ignores shift"))

; type-char : Editor -> Editor
; Inserts the key pressed before the cursor
;Example:
(begin-for-test
  (check-equal? (edit (make-editor "ashish" "kumar") "s") 
                (make-editor "ashishs" "kumar")
                "inserts s to the left of cursor "))
;STRATEGY:
;data decomposition of e :Editor
(define (type-char e key)
    (if  (or (string-length-max? (beside (editor->image e)(key->image key)))
             (to-be-ignored? key))
          e
         (make-editor (string-append (editor-pre e) key) (editor-post e))))
(begin-for-test
  (check-equal? (edit (make-editor "ashish" "kumar") "s") 
                (make-editor "ashishs" "kumar")
                "inserts s to the left of cursor ")
  (check-equal? (edit (make-editor "" "kumar") "a") 
                (make-editor "a" "kumar")
                "inserts a to the left of cursor")
  (check-equal? (edit (make-editor "" "kumar") "\t")
                (make-editor "" "kumar")
                "ignores tab")
  
  (check-equal? (edit (make-editor "ashish" "kumar") "\r") 
                (make-editor "ashish" "kumar")
                "ignores enter")
  (check-equal? (edit (make-editor "ashish" "kumar") "\u007f") 
                (make-editor "ashish" "kumar")
                "ignores delete"))

;to-be-ignored? : Key -> Boolean
; returns true if tab or delete key is pressed
; WHERE:
; key is any single character length key 
;Example:
  (begin-for-test
    (check-equal? (edit (make-editor "" "kumar") "\t")
                (make-editor "" "kumar")
                "ignores tab")
  
     (check-equal? (edit (make-editor "ashish" "kumar") "\r") 
                (make-editor "ashish" "kumar")
                "ignores enter"))
;STRATEGY: function Composition  
(define (to-be-ignored? key )
  (or (string=? "\t" key) (string=? "\u007F" key) (string=? "\r" key)))

;string-length-max? : Image -> boolean
;Will return true if size of the string exceeds the editor area
;WHERE img is any image 
(begin-for-test
  (check-equal? (string-length-max? 
                 (text "abcdefghijklmnopqrstuvwxyzabcdefgh" TEXTSIZE TEXTCOLOR))
                                    #true
                "STring exceeds the editor size")
  (check-equal? (string-length-max? 
                 (text "abcd" TEXTSIZE TEXTCOLOR))
                                    #false
                "STring within the editor size"))
;STRATEGY: function Composition
(define (string-length-max? img)
  (>= (image-width img) WIDTH))


; move-cursor-left : Editor -> Editor
; Move the cursor one character towards left
(begin-for-test
  (check-equal? (edit (make-editor "ashis" "kumar") "left")
                (make-editor "ashi" "skumar")
                "moves cursor left"))
;STRATEGY:
;data decomposition of e: Editor  
(define (move-cursor-left e)
  (if (null-string? (editor-pre e)) e 
      (make-editor (string-remove-last (editor-pre e)) 
                   (string-append (string-last (editor-pre e)) 
                                  (editor-post e)))))

(begin-for-test
  (check-equal? (edit (make-editor "ashis" "kumar") "left")
                (make-editor "ashi" "skumar")
                "moves cursor left")
  (check-equal? (edit (make-editor "as" "kumar") "left") 
                (make-editor "a" "skumar")
                "moves cursor left")
  (check-equal? (edit (make-editor "" "kumar") "left")
                (make-editor "" "kumar")
                "cursor cannot go left"))


; move-cursor-right : Editor -> Editor
; Move the cursor one character towards right
(begin-for-test
  (check-equal? (edit (make-editor "ashis" "kumar") "right") 
                (make-editor "ashisk" "umar")
                "moves cursor right"))
;STRATEGY:
;data decomposition of e: Editor
(define (move-cursor-right e)
  (if (null-string? (editor-post e)) e 
      (make-editor (string-append (editor-pre e) 
                                  (string-first (editor-post e)))
                   (string-rest (editor-post e)))))
(begin-for-test
  (check-equal? (edit (make-editor "ashis" "kumar") "right") 
                (make-editor "ashisk" "umar")
                "moves cursor right")
  (check-equal? (edit (make-editor "as" "kumar") "right")
                (make-editor "ask" "umar")
                "moves cursor right")
  (check-equal? (edit (make-editor "kumar" "") "right") 
                (make-editor "kumar" "")
                "move cursor right"))


; delete-char : Editor -> Editor
; Deletes a character that is just before the cursor

(begin-for-test
  (check-equal? (edit (make-editor "aji" "kumar") "\b") 
                (make-editor "aj" "kumar")
                "deletes i"))
;STRATEGY:
;data decomposition of e: Editor
(define (delete-char e)
  (if (null-string? (editor-pre e)) e
  (make-editor (string-remove-last (editor-pre e)) (editor-post e))))

(begin-for-test
  (check-equal? (edit (make-editor "ashis" "kumar") "\b") 
                (make-editor "ashi" "kumar")
                "deletes s")
  (check-equal? (edit (make-editor "as" "kumar") "\b") 
                (make-editor "a" "kumar")
                "deletes s")
  (check-equal? (edit (make-editor "" "k") "\b")
                (make-editor "" "k")
                "deletes nothing"))



;String Operation Functions
;-------------------------------------------------------------------------------

;string-first : string -> Character
;Will take a string as input and return first character as output
;WHERE str is non empty string
;Example:
; (string-first "ash") -> "a"
;STRATEGY:function composition
(define (string-first str)
  (string-ith str 0))
(begin-for-test
  (check-equal? (string-first "ashiihs") "a" "first character"))

;string-last : string -> character
;Will take a string as input and return last character as output
;WHERE str is non empty string
;Example:
;(string-last "kum") -> "m"
;STRATEGY: function composition
(define (string-last str)
  (string-ith str (- (string-length str) 1)))
(begin-for-test
  (check-equal? (string-last "ashihs") "s" "last character"))

;string-rest : String -> string 
;will delete a first character from a string
;WHERE str is non empty string
;Example:
;(string-rest "ash") -> "sh")
;STRATEGY: function composition
(define (string-rest str )
   (substring str 1  (string-length str) ))
(begin-for-test
  (check-equal? (string-rest "ashish" ) "shish" "rest of the string"))

;string-remove-last : String -> string 
;will delete a last character from a string
;WHERE str is non empty string
;Example:
;(string-remove-last "ashish") -> "ashis")
;STRATEGY: function composition

(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))
(begin-for-test
  (check-equal? (string-remove-last "ashish" ) "ashis" "remove string last"))

;null-string? : String -> Boolean
; checks wether the string is ""
(begin-for-test
  (check-equal? (null-string? "") #true)
  (check-equal? (null-string? "sa") #false))
(define (null-string? str)
  (eq? (string-length str) 0))



