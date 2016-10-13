;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xexpr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/batch-io)

(check-location "06" "xexpr.rkt")

(provide xexpr-element?)
(provide xexpr-tag)
(provide xexpr-attributes)
(provide xexpr-content)
(provide attribute-value)
(provide get-value)
(provide xexpr-find)
(provide xexpr-depth)
(provide xexpr-elements)

; CONSTANTS
;---------------------------------------------------------------------------

(define TIME-ON-TASK 15) ;Hours
(define PREFIX "https://www.google.com/finance?q=")
(define ZERO 0)
(define STR "string1") ; string constant for testing
(define NUM 123) ; number constant for testing
(define LOA1 '((lang1 "en-US")
               (lang2 "en-UK")
               (lang3 "en-FR")
               (lang4 "en-GR"))); ListOf<Attributes>
(define LOA2 '((width "100")
               (height "200")
               (src "../../img.jpg")
               (padding "300")
               (padding "800"))); ListOf<Attributes>


; DATA DEFINITION
;---------------------------------------------------------------------------

; A Tag is a Symbol, representing the name of an XML element.
(define H1 'h1)
(define P 'p)
(define BR 'br)

;---------------------------------------------------------------------------

; An Attribute is a (list AttrName AttrValue)
; representing an attribute name-value pair in an XML element.
(define HREF-NEU (list 'href "http://northeastern.edu"))
(define IMG-SRC (list 'src "ball.gif"))


; Template
;(define (attribute-fn atr)
;  (cond
;    [(empty? atr) ...]
;    [else (... (first atr) ... (second atr)...)]))

;---------------------------------------------------------------------------

; An AttrName is a Symbol, 
; representing the name of an XML attribute.

;---------------------------------------------------------------------------

; An AttrValue is a String, 
; representing the value associated with an XML attribute.

;------------------------------------------------------------------------------

; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)
; INTERP: Represents an XML element.

; Example:
(define HTML-EMPTY 
  '(html 
    ((lang "en-US")) 
    (body 
     (p) 
     (br) 
     (p) 
     (br))))

(define HTML-WITH-TEXT
  '(html
    ((lang "en-US"))
    (body
     (p "Here is assignment " 5)
     (br)
     (p "This is a " 
        (b "data") 
        " definition."))))

(define HTML-WITH-TEXT-TEST
  '(html
    ((lang "en-US")
     (lang "en-UK")
     (lang2 "en-FR")
     (lang "en-GR"))
    (body 
     ((lang2 "en-US")
      (lang "en-FR")
      (lang "en-FR")
      (lang "en-SW"))
          (p "Here is assignment " 5)
          (br)
          (p "This is a " 
             (b "data")
             " definition."))))

(define IMG 
  '(img
    ((src "balls.png")
     (width "100")
     (height "50"))))

(define HTML-NO-ATTRIBUTE
  '(html
    (body 
     (p) 
     (br))))

(define XEXPR-FOR-TEST
  '(html
    ((lang1 "en-US")
     (lang2 "en-UK"))
    (body
     ((lang2 "en-US")
      (lang "en-FR"))
          (p "Here is assignment " 5))))


;; Template
;(define (xexpr-fn xe)
;  (cond
;    [(symbol? xe) ...]
;    [(string? xe) ...]
;    [(number? xe) ...]
;    [(rest-has-list-of-lists? xe) ...]
;    [else ...]))


; FUNCTIONS
;--------------------------------------------------------------------------

; rest-has-list-of-lists? : ListOf<Any> -> Boolean
; returns True if the first element of (rest xe) is a list of lists
; Example:
(begin-for-test
  (check-true (rest-has-list-of-lists? HTML-EMPTY)
              "HTML-EMPTY has an attribute list"))

; STRATEGY : Data Decomposition on xe : ListOf<Any>
(define (rest-has-list-of-lists? xe)
  (local (; checks if (rest xe) is empty
          (define ls 
            (if 
             (empty? xe) 
             '() 
             (rest xe))))
    (cond
      [(empty? ls) false]
      [else (or (empty? (first ls))
                (and 
                 (not (or 
                       (symbol? (first ls))
                       (string? (first ls))
                       (number? (first ls))))
                 (cons? (first ls))
                 (cons? (first (first ls)))))])))

; Testing
(begin-for-test
  (check-false (rest-has-list-of-lists? '())
               "No attributes")
  (check-false (rest-has-list-of-lists? HTML-NO-ATTRIBUTE)
               "No attributes"))

;--------------------------------------------------------------------------

; attribute-list? : ListOf<Any> -> Boolean
; Returns true if the given list is a list of attributes
; Example:
(begin-for-test
  (check-true (attribute-list? LOA1)
              "Is an attribute list"))
; STRATEGY : Data Decomposition on l : ListOf<Any>

(define (attribute-list? l)
  (local (; attribute?: Any -> Boolean
          ; checks if the given value is an attribute
          ; Function composition
          (define (attribute? atr)
            (and
             (not (empty? atr))
             (and (cons? atr)
                  (= (length atr) 2) 
                  (symbol? (first atr))
                  (string? (second atr))))))
    
    
    (cond
      [(empty? l) true]
      [else (andmap attribute? l)])))

; Testing
(begin-for-test
  (check-true (attribute-list? LOA2)
              "Is an attribute list")
  (check-false (attribute-list? (list 'a 'b))
               "Is not an attribute list")
  (check-false (attribute-list? HTML-EMPTY)
               "Is not an attribute list")
  (check-false (attribute-list? '(()))
               "Is not an attribute list"))




;--------------------------------------------------------------------------
; has-tag? : ListOf<Any> -> Boolean
; Return True if Xexpr contains Tag 
; Example: 
(begin-for-test
  (check-true
   (has-tag? HTML-WITH-TEXT)
   "HTML-WITH-TEXT has tags"))

;STRATEGY: Data Decomposition of x : ListOf<Any>
(define (has-tag? x)
  (cond
    [(empty? x) false]
    [else (symbol? (first x))]))

; Testing
(begin-for-test
  (check-false
   (has-tag? '())
   "No Tag"))


;--------------------------------------------------------------------------
; xexpr-element? : Any -> Boolean
; Returns true of x is a valid XML element.
; Example:
(begin-for-test 
  (check-true (xexpr-element? HTML-WITH-TEXT-TEST)
              "Valid XML element"))

; STRATEGY : Function Composition 
(define (xexpr-element? xe) 
  (local (
          ;xexpr-loc? Any -> Boolean 
          ;return true if it is a valid Xexpr
          ;STRATEGY : 
          (define (xexpr-loc? x)
            (or (symbol? x) 
                (string? x) 
                (number? x)
                (and (cons? x) 
                     (has-tag? x)
                     (rest-has-list-of-lists? x) 
                     (attribute-list? (second x))
                     (xexpr-list? (rest (rest x))))
                (and (cons? x) 
                     (has-tag? x) 
                     (xexpr-list? (rest x)))))
          
          ;xexpr-list? Any -> Boolean
          ;return true if its valid xexpr 
          (define (xexpr-list? l)
            (cond 
              [(empty? l) true]
              [else (andmap xexpr-loc? l)])))
    
    (xexpr-loc? xe)))

; Testing
(begin-for-test 
  (check-true  (xexpr-element? HTML-WITH-TEXT)
               "valid XML element")
  (check-true (xexpr-element? HTML-EMPTY)
              "valid XML element")
  (check-true (xexpr-element? H1)
              "valid XML element")
  (check-false (xexpr-element? '(invalid-xexpr1 () () ))
               "invalid : two empty list")
  (check-false (xexpr-element? '(invalid-xexpr2 (()) ))
               "invalid : list containing empty ")
  (check-false (xexpr-element? (list 1 3))
               "invalid : list starting without tag"))




;--------------------------------------------------------------------------
; xexpr-tag : Xexpr -> Maybe<Tag>
; Returns the tag of element xe.
; example:
(begin-for-test
  (check-equal? (xexpr-tag HTML-WITH-TEXT)
                'html
                "tag of HTML-WITHTEXT"))
;STRATEGY: Data Decomposition on xe : Xexpr
(define (xexpr-tag xe) 
  (cond
    [(symbol? xe) false]
    [(string? xe) false]
    [(number? xe) false]
    [else (first xe)]))

;Testing:
(begin-for-test
  (check-equal? (xexpr-tag HTML-WITH-TEXT)
                'html
                "tag of HTML-WITHTEXT")
  (check-equal? (xexpr-tag IMG)
                'img
                "tag of IMG")
  (check-false (xexpr-tag H1)
               "invalid tag")
  (check-false (xexpr-tag STR)
               "invalid tag")
  (check-false (xexpr-tag NUM)
               "invalid tag"))


;--------------------------------------------------------------------------
; xexpr-attributes : Xexpr -> ListOf<Attribute>
; Returns the attributes of the given XML element. 
; example: 
(begin-for-test
  (check-equal? 
   (xexpr-attributes IMG)
   (list
    (list 'src "balls.png")
    (list 'width "100")
    (list 'height "50"))))

;STRATEGY : Data Decomposition on xe : Xexpr
(define (xexpr-attributes xe) 
  (cond
    [(symbol? xe) '()]
    [(string? xe) '()]
    [(number? xe) '()]
    [(rest-has-list-of-lists? xe) (first (rest xe))]
    [else '()]))

;Testing
(begin-for-test
  (check-equal? 
   (xexpr-attributes IMG)
   (list
    (list 'src "balls.png")
    (list 'width "100")
    (list 'height "50")))
  (check-equal? (xexpr-attributes H1)
                empty
                "symbol has no attributes")
  (check-equal? (xexpr-attributes NUM)
                empty
                "number has no attributes")
  (check-equal? (xexpr-attributes STR)
                empty
                "string has no attributes")
  (check-equal? (xexpr-attributes HTML-NO-ATTRIBUTE)
                empty
                "HTML-NO-ATTRIBUTE has no attributes"))


;--------------------------------------------------------------------------
; xexpr-content : Xexpr -> ListOf<Xexpr>
; Extracts the content of an XML element.
; An element's tag and attributes are not part of its content.
; Example :
(begin-for-test
  (check-equal? 
   ( xexpr-content HTML-WITH-TEXT)
   (list
    (list
     'body
     (list 'p "Here is assignment " 5)
     (list 'br)
     (list
      'p
      "This is a "
      (list 'b "data")
      " definition.")))
   "content of HTML-WITH-TEXT"))

; Data Decomposition on xe : Xexpr
(define (xexpr-content xe) 
  (cond
    [(symbol? xe) (list xe)]
    [(string? xe) (list xe)]
    [(number? xe) (list xe)]
    [(rest-has-list-of-lists? xe) (rest (rest xe))]
    [else (rest xe)]))

; Testing 
(begin-for-test
  
  (check-equal? (xexpr-content H1)
                (list H1)
                "symbol has no content")
  (check-equal? (xexpr-content NUM)
                (list NUM)
                "number has no content")
  (check-equal? (xexpr-content STR)
                (list STR)
                "string has no content")
  (check-equal? ( xexpr-content HTML-NO-ATTRIBUTE)
                (list (list 'body (list 'p) (list 'br)))
                "content of HTML-NO-ATTRIBUTES"))

;--------------------------------------------------------------------------

; attribute-value : ListOf<Attribute> AttrName -> Maybe<AttrValue>
; Returns the value of first attribute in loa named aname.
; Returns #f if no attribute in loa has the name aname.
; Example :
(begin-for-test
  (check-equal?
   (attribute-value LOA1 'lang1 )
   "en-US"))
;STRATEGY: Function Composition
(define (attribute-value loa aname) 
  (local (
          ; get-attr-value : Attribute Maybe<AttrValue> -> Maybe<AttrValue>
          ; Returns either AttrValue or False
          ; STRATEGY : Data Decomposition on attr : Maybe<AttrValue>
          (define (get-attr-value attr b)
            (if (eq? (first attr) 
                     aname)
                (second attr)
                b)))
    ;--IN--
    (foldr get-attr-value false loa)))
;--------------------------------------------------------------------------
; list-attribute-value : ListOf<Attribute> AttrName -> ListOf<String>
; Returns the value of all attributes in loa named aname.
; Returns '() if no attribute in loa has the name aname.
;Example
(begin-for-test
  (check-equal?
   (list-attribute-value LOA2 'padding)
   (list "300" "800")
   "list of values with name 'padding"))

;STRATEGY: Function Composition
(define (list-attribute-value loa aname) 
  (local (
          ; get-attr-values : Attribute ListOf<AttrValue> -> ListOf<AttrValue>
          ; Returns either AttrValue or False
          ; STRATEGY : Data Decomposition on attr : ListOf<AttrValue>
          (define (get-attr-values attr b)
            (if (eq? (first attr) 
                     aname) 
                (append (list 
                         (second attr)) 
                        b)
                b)))
    ;--IN--
    (foldr get-attr-values '() loa)))
;Testing
(begin-for-test
  (check-equal?
   (list-attribute-value LOA1 'lang1)
   (list "en-US")
   "list of values with name 'lang1"))

;--------------------------------------------------------------------------
; get-value : Xexpr AttrName -> Maybe<AttrValue>
; Returns the value of the first attribute in xe named aname.
; Example:
(begin-for-test
  (check-equal? 
   (get-value HTML-WITH-TEXT-TEST 'lang2)
   "en-FR"
   "value of lang2"))
; STRATEGY: Data Decomposition on xe : Xexpr
(define (get-value xe aname) 
  
  (cond
    [(symbol? xe) false]
    [(string? xe) false]
    [(number? xe) false]
    [(rest-has-list-of-lists? xe) (attribute-value (second xe) aname)]
    [else false]))

; Testing
(begin-for-test
  (check-false (get-value HTML-NO-ATTRIBUTE '2)
               "invalid aname")
  (check-false (get-value H1 'lang1)
               "invalid aname")
  (check-false (get-value NUM 'lang1)
               "invalid aname")
  (check-false (get-value STR 'lang1)
               "invalid aname")
  (check-equal? (get-value HTML-WITH-TEXT 'lang)
                "en-US"
                "value of 'lang"))


;--------------------------------------------------------------------------
; xexpr-find : Xexpr AttrName -> ListOf<AttrValue>
; Searches xe and nested elements in xe for attributes named aname
; and returns all the values for these attributes.
; Example
(begin-for-test
  (check-equal? (xexpr-find HTML-WITH-TEXT-TEST 'lang)
                (list
                 "en-US"
                 "en-UK"
                 "en-GR"
                 "en-FR"
                 "en-FR"
                 "en-SW")
                "All Values of 'lang"))

;STRATEGY : Data decomposition on exp : Xexpr 
(define (xexpr-find exp aname)
  (local (; list-xexpr-find : ListOf<Xexpr> -> ListOf<AttrValue>
          ; Appends and returns the list of attribute values
          ; Strategy Function composition
          (define (list-xexpr-find l aname)
            (foldr (λ (exp b) (append (xexpr-find exp aname) b)) '() l)))
  ;--IN--
  (cond
    [(symbol? exp) '()]
    [(string? exp) '()]
    [(number? exp) '()]
    [(rest-has-list-of-lists? exp) 
     (append 
      (list-attribute-value (second exp) aname)
      (list-xexpr-find (rest (rest exp)) aname))]
    [else (list-xexpr-find (rest exp) aname)])))



;--------------------------------------------------------------------------
; xexpr-depth : Xexpr -> Natural
; Returns the depth of the deepest nested Xexpr. 
; An Xexpr with no nested elements has depth 0.
; Example
(begin-for-test
  (check-equal? (xexpr-depth HTML-WITH-TEXT-TEST)
                4
                "depth of HTML_WITH-TEXT"))

;STRATEGY : Data decomposition on xe : Xexpr
(define (xexpr-depth xe) 
  (local (; list-xexpr-depth :  ListOf<Xexpr> -> NonNegInt
          ; Returns the depth of the deepest nested Xexpr
          ; ; Strategy: Function composition
          (define (list-xexpr-depth l)
            (local (; find-deepest : Xexpr NonNegInt -> NonNegInt
                    ; compares the depth of x and b and returns the 
                    ; greater of the two
                    ; Strategy: Function composition
                    (define (find-deepest x b)
                        (if (> (xexpr-depth x) b)
                            (xexpr-depth x)
                            b)))
              (foldr find-deepest ZERO l))))
    ;--IN--
    (cond
              [(symbol? xe) ZERO]
              [(string? xe) ZERO]
              [(number? xe) ZERO]
              [(rest-has-list-of-lists? xe) 
               (add1 (list-xexpr-depth (rest (rest xe))))]
              [else (add1 (list-xexpr-depth (rest xe)))])))
;Testing:
(begin-for-test
  (check-equal? (xexpr-depth H1)
                0
                "no depth of Symbol")
  (check-equal? (xexpr-depth NUM)
                0
                "no depth of Number")
  (check-equal? (xexpr-depth STR)
                0
                "no depth of String")
  (check-equal? (xexpr-depth HTML-WITH-TEXT)
                4
                "depth of HTML-WITH-TEXT"))


;--------------------------------------------------------------------------
; xexpr-elements : Xexpr Tag -> ListOf<Xexpr>
; Returns a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with 
; the same tag and this function returns both.
;Example
(begin-for-test
  (check-equal?
   (xexpr-elements HTML-WITH-TEXT-TEST 'b)
   (list (list 'b "data"))
   "all elements of 'b"))
;STRATEGY : Function Composition

(define (xexpr-elements xe t) 
  (local ((define (xexpr-loc-ele exp)
            (cond
              [(symbol? exp) '()]
              [(string? exp) '()]
              [(number? exp) '()]
              [(rest-has-list-of-lists? exp) 
               (if (eq? (first exp) t)
                   (cons  exp (list-xexpr-ele (rest (rest exp))))
                   (list-xexpr-ele (rest (rest exp))))]
              [else (if (eq? (first exp) t)
                        (cons exp (list-xexpr-ele (rest exp)))
                        (list-xexpr-ele (rest exp)))]))
          
          
          
          (define (list-xexpr-ele l)
            (local ((define (fold-help exp b)
                      (append (xexpr-loc-ele exp) b)))
              (foldr fold-help '() l))))
    ;--IN--
    (xexpr-loc-ele xe)))

;Testing:
(begin-for-test
  (check-equal?
   (xexpr-elements HTML-WITH-TEXT-TEST 'body)
   (list
    (list
     'body
     (list
      (list 'lang2 "en-US")
      (list 'lang "en-FR")
      (list 'lang "en-FR")
      (list 'lang "en-SW"))
     (list 'p "Here is assignment " 5)
     (list 'br)
     (list
      'p
      "This is a "
      (list 'b "data")
      " definition.")))
   "all elements of 'body"))


;--------------------------------------------------------------------------
;get-loa-if-name=val : ListOf<Attributes> AttrName AttrValue
;;                     -> ListOf<Attributes>
; Checks if the given list of attributes contains an attribute with name 
; aname and the given value. If true it returns the given list of attributes 
; else returns empty
; Example:
(begin-for-test
  (check-equal? (get-loa-if-name=val LOA1 'lang1 "en-US")
                LOA1
                "contains 'lang1 = 'en-US'")
  (check-equal? (get-loa-if-name=val LOA1 'lang1 "en-R")
                empty
                "does not contain 'lang1 = 'en-R'"))
;STRATEGY: Function Composition
(define (get-loa-if-name=val loa aname val)
  (local (; check-attr-value : Attribute ListOf<Attributes> 
          ;                  -> ListOf<Attributes>
          ; Will return list of valid attributes
          ; STRATEGY : Data Decomposition on attr : Attribute
          (define (check-attr-value attr b)
            (if (and (eq? (first attr) aname)
                     (equal? (second attr) val))
                (append loa b)
                b)))
    (foldr check-attr-value '()  loa)))




;--- The following function requires an internet connection, hence no 
;    code coverage.


;--------------------------------------------------------------------------
; get-attr-ls-for : AttrName AttrValue String -> ListOf<Attributes>
; Will return a list of appened attribute list for the provided company name
; which contains the attribute name and attribute value
; STRATEGY : Function Composition
(define (get-attr-ls-for aname value company) 
  (local (; retrived stock data of Company
          (define stkX (retrieve-stock-data company))
          ; All the elements of 'meta  tag
          (define xe (xexpr-elements stkX 'meta))
          (define (xexpr-loc exp)
            (cond
              [(symbol? exp) '()]
              [(string? exp) '()]
              [(number? exp) '()]
              [(rest-has-list-of-lists? exp)
               (append (get-loa-if-name=val (first(rest exp)) aname value)
                       (list-xexpr (rest(rest exp))))]
              [else (list-xexpr (rest exp))]))
          
          (define (list-xexpr l)
            (local ((define (fold-help exp b)
                      (append (xexpr-loc exp) b)))
              (foldr fold-help '() l))))
    ;--IN--
    (xexpr-loc xe)))


;--------------------------------------------------------------------------
; retrieve-stock-data : String -> String 
; Will retrieve stock data of company in form of XML
; STRATEGY: Function Composition
(define (retrieve-stock-data company)
  (local (; returns the xml of google finance for the given company
          (define url 
            (string-append PREFIX company)))
    (if (url-exists? url)
        (read-xexpr/web 
         (string-append PREFIX company))
        '()))) 

;--------------------------------------------------------------------------
; get-stock-price-for : String -> String
; Will return Stock price of the Company in form of string
;; call this with the company name as String to retrieve stock prices
; STRATEGY : FUnction Composition
(define (get-stock-price-for company)
  (local ((define pr-attr-ls (get-attr-ls-for 'itemprop "price" company))
          (define pr-change-att-ls (get-attr-ls-for 'itemprop 
                                                    "priceChange" 
                                                    company))
          (define price (attribute-value pr-attr-ls 'content))
          (define priceChange (attribute-value pr-change-att-ls 'content)))
    
    (if (or (equal? price false) (equal? priceChange false))
        false
        (string-append company ": Price: " price " Price Change: " 
                       priceChange))))

;--------------------------------------------------------------------------

; Alternate data definition

; An Xexpr is one of:
; - Symbol
; - XWord
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)
; INTERP: Represents an XML element.

; A XWord is '(word ((text String))).

; Functions effected

; 1) xexpr-depth : is effected because we would have to treat Xword as an
;    element with no depth, even though it is a list. We would have to have 
;    extra checks in place to account for this. Also, since attributes and the 
;    second part of Xword have a similar structures, we would have to have 
;    conditions to differentiate between the two.

; 2) xexpr-elements : Returns list of all xml elements with the given tag.
;    Since a tag can have any value, we would have to make changes in 
;    xexpr-elements to differentiate between  '(Word ListOf<Xexpr>) and
;    '(Word ((text String))



;(get-stock-price-for "apple")
;(get-stock-price-for "ford")



