;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname words) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(define  TIME-ON-TASK 20)

(check-location "04" "words.rkt")

(provide arrangements)
(provide insert-everywhere/in-all-words)
(provide arrangement-main)


;1String is a String of single length

; A Word is one of : 
; – '() or
; – (cons 1String Word)

;INTERP: empty is a list with no elemnets
;(cons 1String word) represents a list of single length string 
; where 1String is the first element and  word is the rest of the elements

;examples of  
(define FOD 
  (cons "f" (cons "o" (cons "d" '()))))
(define GOOD 
  (cons "g" (cons "o" (cons "o"(cons "d" '())))))
;(define DAY
;  (cons "d"(cons "a"(cons"y" '()))))
(define GOODY 
  (cons "g" (cons "o" (cons "o"(cons "d" (cons "y" '()))))))
(define FOODY 
  (cons "f" (cons "o" (cons "o"(cons "d" (cons "y" '()))))))


;;TEMPLATE:
;(define (word-fn l)
;  (cond 
;    [(empty? l)  '()]
;    [(cons? l)
;     (... (first l) ...  
;          (word-fn (rest l )) ... ) ]))
;


; List-of-Strings is one of :
; - '() or
; - (cons String List-of-Strings)

;INTERP: empty is a list with no elemnets
;(cons String List-of-Strings) represents a list of words  
; where String is the first element and  List-of-Strings is
;the rest of the elements


;examples of  
(define WORD-FOD 
  (cons "fod" (cons "odf" (cons "ofd"(cons "dof" '())))))
;
;;TEMPLATE:
;(define (word-list-fn l)
;  (cond 
;    [(empty? l)  '()]
;    [(cons? l)
;     (... (first l) ...  
;          (word-list-fn (rest l )) ... ) ]))


; List-of-Words is one of :
; - '() or
; - (cons Word List-of-Words)

;INTERP: empty is a list with no elemnets
;(cons Word List-of-Words) represents a list of list of 1String
; where word is the first element and  List-of-Words is the rest of elements


;examples of  
(define LIST-PO 
  (list (cons "p" (cons "o" '()))  (cons "o"(cons "p" '()))))

;;TEMPLATE:
;(define (List-of-Words-fn l)
;  (cond 
;    [(empty? l)  '()]
;    [(cons? l)
;     (... (first l) ...  
;          (List-of-Words-fn (rest l )) ... ) ]))



; Function Implementation
;-------------------------------------------------------------------------------

;arrangement-main: String -> List-of-Strings
;Will return List of words that are formed by arranging letters from word
;Example
(begin-for-test 
  (check-equal? (arrangement-main "as")
                (list "as" "sa" )
                "list of Words"))
;STRATEGY: Function Composition
(define (arrangement-main str)
  (list-implode(arrangements (explode str))))

;arrangements: Word -> List-of-Words
;will return list of words
;Example 
(begin-for-test
  (check-equal? (arrangements (explode "po"))
                (list (list "p" "o") (list "o" "p"))
                "List-of-Words"))
;STRATEGY : Data Decomposition of w: Word                
(define (arrangements w )
  (cond
    [(empty? w ) (list '()) ]
    [ else  (insert-everywhere/in-all-words (first w )
(arrangements (rest w )) ) ]))


;insert-everywhere/in-all-words : 1String List-of-Words -> List-of-Words
;will return List of Words after placing s in every Location
;Example
(begin-for-test
  (check-equal? (insert-everywhere/in-all-words "p" LIST-PO)
                (list
                 (list "p" "p" "o")
                 (list "p" "p" "o")
                 (list "p" "o" "p")
                 (list "p" "o" "p")
                 (list "o" "p" "p")
                 (list "o" "p" "p"))
                "new list-of-Lo1S"))
;STRATEGY :Data Decomposition of lw : List-of-Words

(define (insert-everywhere/in-all-words s  lw )
  (cond
    [(empty? lw ) empty]
    [else (append (insert-everywhere s  (first lw ))
          (insert-everywhere/in-all-words s  (rest lw ))) ]))



;insert-everywhere: 1String Word -> List-of-words
; will place s at every location in word
; Example :
(begin-for-test
  (check-equal? (insert-everywhere "g" GOOD)
                (list
                 (list "g" "g" "o" "o" "d")
                 (list "g" "g" "o" "o" "d")
                 (list "g" "o" "g" "o" "d")
                 (list "g" "o" "o" "g" "d")
                 (list "g" "o" "o" "d" "g"))
                " g inserted everywhere"))
;STRATEGY: Function Composition

(define (insert-everywhere s  w )
  (cond
    [(empty? w ) (cons (list-add-first s  w ) empty)]
    [else (append (cons (list-add-first s  w ) empty)
                  (in-between s  w )    
                  (cons (list-add-last s  w ) empty)) ]))

;list-add-first : 1String Word -> Word
; Will place 1String at beginning of the word
;Example 
(begin-for-test
  (check-equal? (list-add-first "g" GOOD)
                (list "g" "g" "o" "o" "d")
                "insert at beginning"))
;STRATEGY : Function Composition
(define (list-add-first s  w )
  (cond
    [(empty? w ) (cons s  empty) ]
    [else (cons s  w ) ]))

;list-add-last:  1String Word -> Word
;will add new 1String to end
;WHERE: list is non empty
;example
(begin-for-test
  (check-equal? (list-add-last "y" GOOD )
                GOODY "added to last"))
;STRATEGY: Data decomposition on l : Word
(define (list-add-last s l)
  (cond
    [(empty? l ) (cons s '())]
    [else (cons (first l) (list-add-last  s (rest l) ))]))




;in-between:  1String Word -> List-of-words
; will add 1String in between words
; example
(begin-for-test
  (check-equal?
   (in-between "s" FOD)
   (list (list "f" "s" "o" "d") (list "f" "o" "s" "d"))
   "added in between"))
;STRATEGY : Data Decomposition on w : Word
(define (in-between s  w )
  (cond
    [(empty? (rest w )) empty]
    [else (cons    (cons (first w ) (cons s  (rest w )))
                   (in-front (first w )(in-between s (rest w )))) ]))


;in-front : 1String List-of-Words -> List-of-Words
;will add string in front of each of List-of-Words
;example
(begin-for-test
  (check-equal? (in-front "f" LIST-PO)
                (list (list "f" "p" "o") (list "f" "o" "p"))
                "add string in front of each of List-of-Words"))
;STRATEGY: Data Decomposition on lw : List-of-Words
(define (in-front w  lw )
  (cond
    [(empty? lw ) empty]
    [else (cons (cons w  (first lw ))  (in-front w 
(rest lw ))) ]))


;list-implode: List-of-Words -> List-of-Strings
;will return List of Words
;example
(begin-for-test
  (check-equal? (list-implode LIST-PO)
                (list "po" "op")
                "List-of-Strings"))
;STRATEGY: Data Decomposition on lw : List-of-Words
(define (list-implode lw)
  (cond
    [(empty? lw)  '()]
    [else (cons (implode (first lw)) (list-implode (rest lw)))]))
                     
;--------------------------------------------------------------------------
;ALternate Data Definition

;1.
;1String is a String of single length

; A Word is one of : 
; – '() or
; – (cons 1String Word)

;INTERP: empty is a list with no elemnets
;(cons 1String word) represents a list of single length string 
; where 1String is the first element and  word is the rest of the elements


; List-of-Strings is one of :
; - '() or
; - (cons String List-of-Strings)

;INTERP: empty is a list with no elemnets
;(cons String List-of-Strings) represents a list of words  
; where String is the first element and  List-of-Strings is
;the rest of the elements

;Using string operation instead of list for adding words to the List-of-Strings 

;Pros: No need of having new data -List-of-Words
;      no need for using Implode function on list-of-words
;Cons :Adding removing a charcter at a position in a word is very easy when 
;      done with list 


;2 using Only strings instead of list of characters

; List-of-Strings is one of :
; - '() or
; - (cons String List-of-Strings)
;INTERP: empty is a list with no elemnets
;(cons String List-of-Strings) represents a list of words  
; where String is the first element and  List-of-Strings is
;the rest of the elements

; Pros : Strings are light weight data . list has constructor for each elements
; Cond : Adding and removing characters from a string is a a complex functions

;--------------------------------------END-------------------------------------