;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xprexr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

(define TIME-ON-TASK 5) ;Hours
(define PREFIX "https://www.google.com/finance?q=")
(define ZERO 0)


; DATA DEFINITION
;---------------------------------------------------------------------------

; An Xexpr is one of:
; - Symbol
; - String
; - Number
; - (cons Tag (cons ListOf<Attribute> ListOf<Xexpr>))
; - (cons Tag ListOf<Xexpr>)
;INTERP: Represents an XML element.

;Example:
(define HTML-EMPTY '(html ((lang "en-US")) (body (p) (br) (p) (br))))
(define HTML-EMPTY3 '(html ((lang "en-US")) (body (p) (br) () (p) (br))))
(define HTML-BAD-AT0 '(html (((lang "en-US"))) (body (p) (br) (p) (br))))
(define HTML-BAD-AT1 '(html (((lang "en-US"))(lang "en-US")) (body (p) (br) (p)
                                                                   (br))))
(define HTML-BAD-AT2 '(html ((lang "en-US") ()) (body (p) (br) (p) (br))))
(define HTML-WITH-TEXT
  '(html
    ((lang "en-US"))
    ((lang "en-UK"))
        (body
     (p "Here is assignment " 5)
     (br)
     (p "This is a " (b "data") " definition."))))
(define HTML-WITH-TEXT-TEST
  '(html
    ((lang "en-US")(lang "en-UK")(lang2 "en-FR")(lang "en-GR"))
    (body ((lang2 "en-US")(lang "en-FR")(lang "en-FR")(lang "en-SW"))
          (p "Here is assignment " 5)
          (br)
          (p "This is a " (b "data") " definition."))))
(define IMG '(img ((src "balls.png") (width "100") (height "50"))))

; TEMPLATE:
(define (xexpr-fn xe)
  (cond
    [(symbol? xe) ...]
    [(string? xe) ...]
    [(number? xe) ...]
    [else (... (xexpr-tag xe)...
          (xexpr-content xe)...
          (xexpr-attributes xe)...)]))

;--------------------------------------------------------------------------

; A Tag is a Symbol, representing the name of an XML element.

;Example:
(define H1 'h1)
(define P 'p)
(define BR 'br)

;--------------------------------------------------------------------------
; An Attribute is a (list AttrName AttrValue)
; representing an attribute name-value pair in an XML element.
(define HREF-NEU (list 'href "http://northeastern.edu"))
(define IMG-SRC (list 'src "ball.gif"))

;; Template
(define (attribute-fn atr)
  (cond
    [(empty? atr) ...]
    [else (... (first atr) ... (second atr)...)]))
;--------------------------------------------------------------------------
; An AttrName is a Symbol, 
; representing the name of an XML attribute.
; An AttrValue is a String, 
; representing the value associated with an XML attribute.


(define (attribute-list? l)
  (local ((define (attribute? atr)
            (cond 
              [(empty? atr) false]
              [else (and (cons? atr)
                         (= (length atr) 2) 
                         (symbol? (first atr))
                         (string? (second atr)))])))
    
    (cond
      [(empty? l) true]
      [else (if (cons? l)
                (andmap attribute? l)
                false)])))



;--------------------------------------------------------------------------


(define (has-tag? x)
  (cond
    [(empty? x) false]
    [else (symbol? (first x))]))

;--------------------------------------------------------------------------
; xexpr-element? : Any -> Boolean
; Returns true of x is a valid XML element.

(define (xexpr-element? xe) 
  (local ((define (xexpr-loc? x)
            (cond
              [(symbol? x) true]
              [(string? x) true]
              [(number? x) true]
              [(has-tag? x) (xexpr-list? (xexpr-content x))]
              [else false]))
            
          
          (define (xexpr-list? l)
            (cond 
              [(empty? l) true]
              [else (andmap xexpr-loc? l)])))
    
    (xexpr-loc? xe)))


; xexpr-tag : Xexpr -> Maybe<Tag>
; Returns the tag of element xe.
(define (xexpr-tag xe) 
  (cond
    [(symbol? xe) false]
    [(string? xe) false]
    [(number? xe) false]
    [else (first xe)]))

; xexpr-attributes : Xexpr -> ListOf<Attribute>
; Returns the attributes of the given XML element. 
(define (xexpr-attributes xe) 
  (cond 
    [(empty? (rest xe)) '()]
    [else (local ((define optional-loa+lexp (rest xe))
                  (define loa-or-lox (first optional-loa+lexp)))
            (if (attribute-list? loa-or-lox)
                loa-or-lox
                '()))]))


; xexpr-content : Xexpr -> ListOf<Xexpr>
; Extracts the content of an XML element.
; An element's tag and attributes are not part of its content.
(define (xexpr-content xe) 
  (local ((define (xexpr-content-loc expr)
            (cond
              [(symbol? expr) '()]
              [(string? expr) '()]
              [(number? expr) '()]
              [else (get-list-of-xexp expr)]))
          
          (define (get-list-of-xexp x)
            (cond 
              [(empty? (rest x)) '()]
              [else (local ((define optional-loa+lexp (rest x))
                            (define len (length x))
                            (define loa-or-lox (first optional-loa+lexp)))
                      (if (attribute-list? loa-or-lox)
                          (rest optional-loa+lexp)
                          optional-loa+lexp))])))
    
    
    (xexpr-content-loc xe)))


; attribute-value : ListOf<Attribute> AttrName -> Maybe<AttrValue>
; Returns the value of first attribute in loa named aname.
; Returns #f if no attribute in loa has the name aname.
(define (attribute-value loa aname) 
  (local ((define (get-attr-value attr b)
            (if (eq? (first attr) aname)
                (second attr)
                b)))
    (foldr get-attr-value false loa)))

; list-attribute-value : ListOf<Attribute> AttrName -> ListOf<String>
; Returns the value of all attributes in loa named aname.
; Returns '() if no attribute in loa has the name aname.
(define (list-attribute-value loa aname) 
  (local ((define (get-attr-value attr b)
            (if (eq? (first attr) aname)
                (append (list (second attr)) b)
                b)))
    (foldr get-attr-value '() loa)))


; get-value : Xexpr AttrName -> Maybe<AttrValue>
; Returns the value of the first attribute in xe named aname.

(define (get-value xe aname) 
  
  (cond
    [(symbol? xe) false]
    [(string? xe) false]
    [(number? xe) false]
    [else (local ((define loa (xexpr-attributes xe)))
            (attribute-value loa aname))]))


; xexpr-find : Xexpr AttrName -> ListOf<AttrValue>
; Searches xe and nested elements in xe for attributes named aname
; and returns all the values for these attributes.
(define (xexpr-find xe aname) 
  (local ((define (xexpr-loc-find exp)
            (cond
              [(symbol? exp) '()]
              [(string? exp) '()]
              [(number? exp) '()]
              [else (local ((define loa (xexpr-attributes exp)))
                      (append (list-attribute-value loa aname)
                              (list-xexpr-find (xexpr-content exp))))]))
          
          (define (list-xexpr-find l)
            (local ((define (fold-help exp b)
                      (append (xexpr-loc-find exp) b)))
            (foldr fold-help '() l))))
    
    (xexpr-loc-find xe)))


; xexpr-depth : Xexpr -> Natural
; Returns the depth of the deepest nested Xexpr. 
; An Xexpr with no nested elements has depth 0.
(define (xexpr-depth xe) 
  (local ((define (xexpr-loc-depth exp)
            (cond
              [(symbol? exp) ZERO]
              [(string? exp) ZERO]
              [(number? exp) ZERO]
              [else (add1 (list-xexpr-depth (xexpr-content exp)))]))
          
          (define (list-xexpr-depth l)
            (local ((define (find-deepest x b)
                      (local ((define x-depth (xexpr-loc-depth x)))
                      (if (> (xexpr-loc-depth x) b)
                          (xexpr-loc-depth x)
                          b))))
              (foldr find-deepest ZERO l))))
    
    (xexpr-loc-depth xe)))

; xexpr-elements : Xexpr Tag -> ListOf<Xexpr>
; Returns a list of all elements in xe whose tag is t.
; An element with tag t may be nested inside another element with 
; the same tag and this function returns both.
(define (xexpr-elements xe t) 
  (local ((define (xexpr-loc-ele exp)
            (cond
              [(symbol? exp) '()]
              [(string? exp) '()]
              [(number? exp) '()]
              [else (local ((define tag (xexpr-tag exp))
                            (define content (xexpr-content exp)))
                           (if (eq? tag t)
                               (list exp)
                               (list-xexpr-ele content)))]))
          
          
          
          (define (list-xexpr-ele l)
            (local ((define (fold-help exp b)
                      (append (xexpr-loc-ele exp) b)))
            (foldr fold-help '() l))))
    
    (xexpr-loc-ele xe)))






(define (get-loa-if-name=val loa aname val)
  (local ((define (check-attr-value attr b)
            (if (and (eq? (first attr) aname)
                     (equal? (second attr) val))
                (append loa b)
                b)))
    (foldr check-attr-value '()  loa)))


(define (get-attr-ls-for aname value company) 
  (local ((define stkX (retrieve-stock-data company))
          (define xe (xexpr-elements stkX 'meta))
          (define (xexpr-loc exp)
            (cond
              [(symbol? exp) '()]
              [(string? exp) '()]
              [(number? exp) '()]
              [else (local ((define loa (xexpr-attributes exp))
                            (define content (xexpr-content exp)))
                      (append (get-loa-if-name=val loa aname value)
                              (list-xexpr content)))]))
          
          (define (list-xexpr l)
            (local ((define (fold-help exp b)
                      (append (xexpr-loc exp) b)))
            (foldr fold-help '() l))))
    
    (xexpr-loc xe)))






(define (retrieve-stock-data company)
  (local ((define url (string-append PREFIX company)))
    (if (url-exists? url)
                        (read-xexpr/web (string-append PREFIX company))
                        '())))



;; call this with the company name as String to retrieve stock prices
(define (get-stock-price-for company)
  (local ((define pr-attr-ls (get-attr-ls-for 'itemprop "price" company))
          (define pr-change-att-ls (get-attr-ls-for 'itemprop 
                                                      "priceChange" 
                                                      "ford"))
          (define price (attribute-value pr-attr-ls 'content))
          (define priceChange (attribute-value pr-change-att-ls 'content)))
    
    (if (or (equal? price false) (equal? priceChange false))
        false
    (string-append company ": Price: " price " Price Change: " priceChange))))



;(xexpr-element? HTML-WITH-TEXT-TEST)
;(xexpr-element? HTML-WITH-TEXT)
;(xexpr-element? HTML-EMPTY)
;(xexpr-element? IMG)

;(xexpr-attributes HTML-WITH-TEXT-TEST)
;(xexpr-attributes HTML-WITH-TEXT)
;(xexpr-attributes HTML-EMPTY)
;(xexpr-attributes IMG)
;
;(xexpr-content HTML-WITH-TEXT-TEST)
;(xexpr-content HTML-WITH-TEXT)
;(xexpr-content HTML-EMPTY)
;(xexpr-content IMG)
;
;(get-value HTML-WITH-TEXT-TEST 'lang2)
;(get-value HTML-WITH-TEXT 'lang)
;(get-value HTML-EMPTY 'lang)
;(get-value IMG 'width)
;(get-value HTML-WITH-TEXT 'random)
;
;
;(xexpr-find HTML-WITH-TEXT-TEST 'lang)
;(xexpr-find IMG 'width)
;(xexpr-find HTML-WITH-TEXT-TEST 'lang2)
;
;
;(xexpr-depth HTML-WITH-TEXT-TEST)
;(xexpr-depth HTML-WITH-TEXT)
;(xexpr-depth HTML-EMPTY)
;(xexpr-depth IMG)
;
;(xexpr-elements HTML-WITH-TEXT-TEST 'b)
;(xexpr-elements HTML-WITH-TEXT-TEST 'body)
;(xexpr-elements HTML-WITH-TEXT-TEST 'p)
;(xexpr-elements HTML-EMPTY 'p)
;(xexpr-elements IMG 'hjhj)

;(read-xexpr/web "https://www.google.com/finance?q=apple")

;(xexpr-element? (read-xexpr/web "https://www.google.com/finance?q=apple"))

;(get-stock-price-for "apple")
;(get-stock-price-for "adjshhdak")
;
;(check-location "06" "xexpr.rkt")

;(attribute-list? HTML-BAD-AT0)
;(attribute-list? HTML-BAD-AT1)
;(attribute-list? HTML-BAD-AT2)