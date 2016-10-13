;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 15)

(provide expr->strings)

;  CONSTANTS
;-----------------------------------------------------------------------------

(define IS-PREFIX true)
(define NOT-PREFIX false)
(define PREFIX-START-STR "(+ ")
(define INFIX-START-STR "(")
(define MT-STR "")
(define CLOSE-STR ")")
(define OPEN-STR "(")
(define SPACE-STR " ")
(define EMPTY-LS '())

;  DATA DEFINITIONS
;------------------------------------------------------------------------------

; An Expr is one of:
; - Integer
;  - (make-add (cons Expr NEListOf<Expr>) Boolean)
;  - (make-mul (cons Expr NEListOf<Expr>) Boolean)
; Represents a numeric expression that is either an integer, or an addition
; or multiplication of two or more expressions.
; The Boolean flag indicates whether the expression uses infix or prefix 
; notation.
(define-struct add (exprs infix?))
(define-struct mul (exprs infix?))

; Data Examples:

(define TEST-EXPR-1
  (make-add (list (make-mul (list 1 2) true)
                  (make-add (list 3 4) false))
            true))

(define TEST-EXPR-2
  (make-add (list (make-mul (list 1 (make-add (list 5 6) false)) false)
                  (make-add (list 3 4) false))
            true))

(define TEST-EXPR-3
  (make-add (list (make-mul (list 1 (make-add (list 5 6) false)) true)
                  (make-add (list 3 4) false)
                  (make-add (list 3 4) false)
                  (make-add (list 3 4) false)
                  (make-add (list (make-add (list 3 4) false) 4) false)
                  (make-add (list (make-add (list 3 4) false) 4) false))
            true))



; Template:
; exprn-fn : Expr -> ???
; (define (exprn-fn ex)
;   (cond
;     [(integer? ex) (... ex ...)]
;     [(add? ex) (... (add-fn ex) ...)]
;     [(mul? ex) (... (mul-fn ex) ...)]))

;------------------------------------------------------------------------------

; Add is (make-add (cons Expr NEListOf<Expr>) Boolean)
; Interpretation: Represents the addition of two or more Expr. The Boolean flag
; indicates whether the expression uses infix or prefix notation.

; Data Examples:
(define ADD-EXAMPLE-1 (make-add (list 3 4) false))
(define ADD-EXAMPLE-2 (make-add (list 3 1 5) true))

; Template:
; add-fn : Add -> ???
; (define (add-fn a)
;   (... (exprNelist-fn (add-exprs a)) ... (add-infix? a) ...))

;------------------------------------------------------------------------------

; Mul is (make-mul (cons Expr NEListOf<Expr>) Boolean)
; Interpretation: Represents the multiplication of two or more Expr. 
; The Boolean flag indicates whether the expression uses infix or prefix 
;notation.

; Data Examples:
(define MUL-EXAMPLE-1 (make-mul (list 3 5) true))
(define MUL-EXAMPLE-2 (make-mul (list 2 5 5) false))

; Template:
; mul-fn : Mul -> ???
; (define (mul-fn m)
;   (... (exprNelist-fn (mul-exprs m)) ... (mul-infix? m) ...))

;------------------------------------------------------------------------------

; An ExprNelist is (cons Expr NEListOf<Expr>)
; ExprNelist is a list of 2 or more Exprs

; Data Example:
(define EXPR-NE-LS 
  (list (make-add (list 1 2) true)
        (make-mul (list 2 3) false)))

; Template:
; exprNelist-fn : ExprNelist -> ???
; (define (exprNelist-fn ls)
;   (...(first ls) ... (rest ls)...))

;------------------------------------------------------------------------------

; OptrString is one of 
; - "+"
; - "*"

(define PLUS "+")
(define MULT "*")

; Template:
; optrString-fn: OptrString -> ???
; (define (optrString-fn op)
;   (cond
;     [(string=? op PLUS) ...]
;     [(string=? op MULT) ...]))

;------------------------------------------------------------------------------

; SExpr is one of
; - (number->string Integer)
; - OptrString
; - (cons OptrString (cons SExpr NEListOf<SExpr>))    --(1)
; - (cons SExpr (cons OptrString NEListOf<SExpr>))    --(2)

; Where: (1) only one OptrString as the first element followed by a 
;        SExprs (except OptrString)
;        (2) does not start or end with an OptrString and the elements of this
;        list alternate between SExpr(except OptrString) and OptrString

(define SE-NUMBER (number->string 81))
(define TEST-SE-1 (list SE-NUMBER PLUS (list SE-NUMBER MULT SE-NUMBER)))
(define TEST-SE-2 (list MULT SE-NUMBER TEST-SE-1 SE-NUMBER))

; Template:
; sExpr-fn: SExpr -> ???
; (define (sExpr-fn esl)
;   (cond
;     [(integer-str? esl) ...]
;     [(optr? esl) ...]
;     [(prefix-ls? esl) (...(sExpr-fn(first esl))...(sExpr-fn (second esl))
;                           ... (sExpr-list-fn (rest (rest))))]
;     [else (...(sExpr-fn (first esl))...(sExpr-fn (second esl))
;               ... (sExpr-list-fn (rest (rest))))]))


; Template for NEListOf<SExpr>
; sExpr-list-fn : NEListOf<SExpr> -> ???
; (define (nelist-sExpr-fn nelst)
;   (cond
;     [(empty? (rest nelst)) (... (sExpr-fn (first nelst)) ...)]
;     [else
;      (... (sExpr-fn (first nelst)) ...
;           (nelist-fn (rest nelst)) ...)]))


;  FUNCTIONS 
;------------------------------------------------------------------------------

; integer-str? SExpr -> Boolean
; returns true if se is of the type (number->string Integer)

; Examples:

(begin-for-test
  (check-true (integer-str? "3")
                "number string")
  (check-false (integer-str? "+")
                "not a number string"))

; Strategy: Function composition

(define (integer-str? se) 
  (and
   (not (empty? se))
   (not (cons? se))
   (integer? (string->number se))))

;------------------------------------------------------------------------------

; prefix-ls? SExpr -> Boolean
; returns true if SExpr is of the type 
; (cons OptrString (cons SExpr NEListOf<SExpr>))

; Examples:

(begin-for-test
  (check-true (prefix-ls? (list "+" "2" "3"))
              "SExpr has prefixed OptrString")
  (check-false (prefix-ls? "+")
                "is not an SExpr with prefixed OptrString"))

; Strategy: Function composition

(define (prefix-ls? sl) 
  (and (cons? sl)
       (not (cons? (first sl)))
       (not (integer? (string->number (first sl))))))

;------------------------------------------------------------------------------

; plus? : Any -> ???
; returns true if op is the OptrString, PLUS.

; Examples

(begin-for-test
  (check-true (plus? "+")
              "op is PLUS")
  (check-false (plus? (list "+" "2" "3"))
                "op is not PLUS"))

; Strategy: Function composition

(define (plus? op) 
  (and (string? op) (string=? op PLUS)))

;------------------------------------------------------------------------------

; mult? : Any -> ???
; returns true if op is the OptrString, MULT.

; Examples

(begin-for-test
  (check-true (mult? "*")
              "op is MULT")
  (check-false (mult? (list "+" "2" "3"))
                "op is not MULT"))

; Strategy: Function composition
(define (mult? op) 
  (and (string? op) (string=? op MULT)))

;------------------------------------------------------------------------------

; optr? : Any -> ???
; returns true if op is an OptrString

; Examples:

(begin-for-test
  (check-true (optr? "+")
              "is an OptrString")
  (check-false (optr? (list 1 2 3))
                "is not an OptrString"))

; Strategy: Function composition

(define (optr? op)
  (or (plus? op) (mult? op)))

;------------------------------------------------------------------------------

; expr->strings : Expr Natural -> ListOf<String>
; Returns a rendering of exp as a sequence of lines,
; where each line is a string not longer than width characters.
; EFFECT: errors if expr cannot fit width

; Examples:

(begin-for-test
  (check-equal? (expr->strings TEST-EXPR-1 100)
                (list "((1 * 2) + (+ 3 4))")
                "unstacked")
  (check-equal? (expr->strings TEST-EXPR-1 9)
                (list "((1 * 2)"
                      " +"
                      " (+ 3 4))")
                "one level stacking")
  (check-equal? (expr->strings TEST-EXPR-1 8)
                (list "((1 * 2)"
                      " +"
                      " (+ 3"
                      "    4))")
                "two level stacking (1 subexpression)")
  (check-equal? (expr->strings TEST-EXPR-1 7)
                (list "((1"
                      "  *"
                      "  2)"
                      " +"
                      " (+ 3"
                      "    4))")
                "two level stacking (2 subexpressions)")
  (check-error (expr->strings TEST-EXPR-1 4) "doesn't fit"))

; Strategy: Function compostion

(define (expr->strings exp width) 
  (sExpr->strs (expr->sExpr exp) width))

;------------------------------------------------------------------------------

; return-spaces : Natural -> String
; Returns a string with n spaces

; Examples:

(begin-for-test
  (check-equal? (return-spaces 3)
                "   "
                "3 spaces")
  (check-equal? (return-spaces 0)
                ""
                "Zero spaces"))

; Strategy : Function composition
(define (return-spaces n)
  (if (= n 0)
      ""
      (implode (build-list n (λ (x) SPACE-STR)))))

;------------------------------------------------------------------------------

; strList-for-width : String String String String Natural -> ListOf<String>
; Appends s1, s2, s3 and s4 into a single string whose length is less
; than width and returns it as a list.
; Effect: errors if the appended string is longer than width

; Examples:

(begin-for-test
  (check-equal? (strList-for-width " " "(" "+" "" 7)
                (list " (+")
                "the length of the string ' (+' is < 7")
  (check-error (strList-for-width "     " "" "3" ")" 5)
               "the length of the string '     3)' is > 5"))

; Strategy: Function composition

(define (strList-for-width s1 s2 s3 s4 width)
  (local (; str is s1, s2, s3 and s4 appended
          (define str (string-append s1 s2 s3 s4))
          ; len is the length of str
          (define len (string-length str)))
    ;--IN--
    (if (> len width)
        (error "Error : Expr could not fit into the given width")
        (list str))))

;------------------------------------------------------------------------------

; fit-possible? : String String SExpr String Natural -> Boolean
; Checks if the string formed by appending ind, pre, esl and end can fit in 
; width
; Where: se is only of the types 
; - (cons SExpr (cons OptrString NEListOf<SExpr>)) Or
; - (cons OptrString (cons SExpr NEListOf<SExpr>))

; Examples:

(begin-for-test
  (check-false (fit-possible? " " "(" TEST-SE-1 "" 7)
               "can not fit in width")
  (check-true (fit-possible? " " "(" TEST-SE-2 "" 100)
              "can fit in width"))

; Strategy: Function composition

(define (fit-possible? ind pre se end width)
  (local (; str is ind, pre, the string representation of se and end appended
          (define str (append-to-str ind pre se end))
          ; len is the length of str
          (define len (string-length str)))
    ;--IN--
    (<= len width)))

;------------------------------------------------------------------------------

; append-to-str : String String String SExpr -> String
; returns a string formed by appending the strings ind and pre, the SExpr se
; and the string end.

; Where: se is only of the types 
; - (cons SExpr (cons OptrString NEListOf<SExpr>)) Or
; - (cons OptrString (cons SExpr NEListOf<SExpr>))

; Examples:

(begin-for-test
  (check-equal? (append-to-str  " " "(" TEST-SE-1 "")
                " ((81 + (81 * 81))"
                "returns appended string")
  (check-equal? (append-to-str  " " "(" TEST-SE-2 ")")
                " ((* 81 (81 + (81 * 81)) 81))"
                "returns appended string"))

; Strategy: Function composition

(define (append-to-str ind pre se end)
  (string-append ind pre (sExprs->string se) end))

;------------------------------------------------------------------------------

; get-indent : Boolean String String -> String
; Returns the indentation required. Here prefix? indicates if the OptrString,
; is prefixed or infixed and ind is the indentation string of the previous line

; Examples:

(begin-for-test
  (check-equal? (get-indent IS-PREFIX "(((" "  ")
                "        "
                "returns indentation string")
  (check-equal? (get-indent NOT-PREFIX "(((" "  ")
                "      "
                "returns indentation string"))

; Strategy: Function composition

(define (get-indent prefix? pre ind)
  (local (;new-pre is pre appended with either prefix or infix start string
          (define new-pre (if prefix? 
                              (string-append pre PREFIX-START-STR)
                              (string-append pre INFIX-START-STR)))
          ;len is the length of str
          (define len (- (string-length new-pre) 1))
          ;sps is a string with 'len' spaces
          (define sps (return-spaces len)))
    ;--IN--
    (string-append sps " " ind)))

;------------------------------------------------------------------------------

; sExpr->strs : SExpr Natural -> ListOf<String>
; Returns a rendering of se0 as a sequence of lines,
; where each line is a string not longer than width characters.
; EFFECT: errors if sexpr cannot fit width

; Examples:

(begin-for-test
  (check-equal? (sExpr->strs TEST-SE-1 10)
                (list "(81" 
                      " +" 
                      " (81" 
                      "  *" 
                      "  81))")
                "returns a list of strings with max length 10")
  (check-error (sExpr->strs TEST-SE-1 4)
               "error: because TEST-SE-1 can't be fit even after splitting")
  (check-equal? (sExpr->strs TEST-SE-2 100)
                (list "(* 81 (81 + (81 * 81)) 81)")
                "returns a list of strings with max length 100")
  (check-equal? (sExpr->strs TEST-SE-2 9)
                (list "(+ 81" 
                      "   (81" 
                      "    +" 
                      "    (81" 
                      "     *" 
                      "     81))" 
                      "   81)")
                "returns a list of strings with max length 9"))

; Strategy: Data decomposition on se : SExpr

(define (sExpr->strs se0 width)
  (local (; sExpr->strs/a : String String SExpr String -> ListOf<String>
          ; Returns a rendering of se as a sequence of lines,
          ; where each line is a string not longer than width characters. 
          
          ; Where: (string-append ind pre) is the string to be added to the 
          ; begining and end is the string to be added to the end of the output
          ; of se0 processed so far
          ; Effect : errors if sexpr cannot fit width
          ; Strategy: Data decomposition on se: SExpr
          (define (sExpr->strs/a pre ind se end)
            (cond
              [(integer-str? se) (strList-for-width ind pre se end width)]
              [(optr? se) (strList-for-width ind pre se MT-STR width)]
              [(prefix-ls? se) 
               (local (;str is ind, pre, the string form of se and end appended
                       (define str (append-to-str ind pre se end))
                       ;new-pre is the updated pre accumulator
                       (define new-pre (string-append pre PREFIX-START-STR))
                       ;new-ind is the updated ind accumulator
                       (define new-ind (get-indent IS-PREFIX pre ind)))
                 (if (fit-possible? ind pre se end width)
                     (list str)
                     (append (sExpr->strs/a new-pre ind (second se) MT-STR)
                             (sEx-ls->strs (rest (rest se)) new-ind end))))]
              [else 
               (local (;str is ind, pre, the string form of se and end appended
                       (define str (append-to-str ind pre se end))
                       ;new-pre is the updated pre accumulator
                       (define new-pre (string-append pre INFIX-START-STR))
                       ;new-ind is the updated ind accumulator
                       (define new-ind (get-indent NOT-PREFIX pre ind)))
                 (if (fit-possible? ind pre se end width)
                     (list str)
                     (append (sExpr->strs/a new-pre ind (first se) MT-STR)
                             (sExpr->strs/a MT-STR new-ind (second se) MT-STR)
                             (sEx-ls->strs (rest (rest se)) new-ind end))))]))
          
          
          ; sEx-ls->strs : NEListOf<SExpr> String String -> ListOf<String>
          ; Returns a rendering of ls as a sequence of lines,
          ; where each line is a string not longer than width characters.
          
          ; Where: ind is the indentation before each SExpr in ls and end is
          ; the string to be appended to the end of the last SExpr in ls
          ; Effect : errors if sexpr cannot fit width
          ; Strategy: Data decomposition on ls: NEListOf<SExpr>
          (define (sEx-ls->strs ls ind end)
            (local ((;n-end is the updated end accumulator
                     define n-end (string-append end CLOSE-STR)))
              (cond
                [(empty? (rest ls)) (sExpr->strs/a MT-STR ind (first ls) 
                                                   n-end)]
                [else (append (sExpr->strs/a MT-STR ind (first ls) MT-STR)
                              (sEx-ls->strs (rest ls) ind MT-STR))]))))
    ;--IN--
    (sExpr->strs/a MT-STR MT-STR se0 MT-STR)))

;------------------------------------------------------------------------------

; sExprs->string NEListOf<SExpr> -> String
; Renders esl0 as a String

; Examples:

(begin-for-test
  (check-equal? (sExprs->string TEST-SE-1)
                "(81 + (81 * 81))"
                "returns a string that represents the NEListOf<SExpr>")
  (check-equal? (sExprs->string TEST-SE-2)
                "(* 81 (81 + (81 * 81)) 81)"
                "returns a string that represents the NEListOf<SExpr>")
  (check-equal? (sExprs->string (list PLUS))
                "(+)"
                "returns a string that represents the NEListOf<SExpr>")
  (check-equal? (sExprs->string (list SE-NUMBER))
                (string-append "(" SE-NUMBER ")")
                "returns a string that represents the NEListOf<SExpr>"))

; Strategy: Function composition

(define (sExprs->string esl0)
  (local (; sExprs->str/a : NEListOf<SExpr> String -> String
          ; Renders esl as a String
          ; Where: a is the string representation of SExpr in esl0 which are not
          ; esl
          ; Strategy: Data decomposition on esl: NEListOf<SExpr>
          (define (sExprs->str/a esl a)
            (cond
              [(empty? (rest esl)) 
               (if (cons? (first esl))
                   (string-append a 
                                  (sExprs->str/a (first esl) OPEN-STR) 
                                  CLOSE-STR)
                   (string-append a (first esl) CLOSE-STR))]
              [else 
               (local (; new-a is the updated accumulator a
                       (define new-a 
                         (if (cons? (first esl))
                             (string-append a 
                                            (sExprs->str/a (first esl) OPEN-STR)
                                            SPACE-STR)
                             (string-append a (first esl) SPACE-STR))))
                 (sExprs->str/a (rest esl) new-a))])))
    ;--IN--
    (sExprs->str/a esl0 OPEN-STR)))

;------------------------------------------------------------------------------

; expr->sExpr : Expr -> SExpr
; Returns a representation of exp as a SExpr

; Examples:

(begin-for-test
  (check-equal? (expr->sExpr TEST-EXPR-1)
                (list
                 (list "1" "*" "2")
                 "+"
                 (list "+" "3" "4"))
                "returns a SExpr that represents the given Expr")
  (check-equal? (expr->sExpr TEST-EXPR-2)
                (list
                 (list "*" "1" (list "+" "5" "6"))
                 "+"
                 (list "+" "3" "4"))
                "returns a SExpr that represents the given Expr"))

; Strategy : Data decomposition on ex : Expr

(define (expr->sExpr ex)
  (cond
    [(integer? ex) (number->string ex)]
    [(add? ex) (add->sExpr ex)]
    [(mul? ex) (mul->sExpr ex)]))

; Tests:

(begin-for-test
  (check-equal? (expr->sExpr TEST-EXPR-3)
                (list
                 (list "1" "*" (list "+" "5" "6"))
                 "+"
                 (list "+" "3" "4")
                 "+"
                 (list "+" "3" "4")
                 "+"
                 (list "+" "3" "4")
                 "+"
                 (list "+" (list "+" "3" "4") "4")
                 "+"
                 (list "+" (list "+" "3" "4") "4"))
                "returns a string that represents the NEListOf<SExpr>"))

;------------------------------------------------------------------------------

; add->sExpr : Add -> SExpr
; Returns a rendering of a as an SExpr

; Examples :

(begin-for-test
  (check-equal? (add->sExpr (make-add (list 3 4) false))
                (list "+" "3" "4")
                "Add with prefixed symbol")
  (check-equal? (add->sExpr (make-add (list 5 6 7 8) true))
                (list "5" "+" "6" "+" "7" "+" "8")
                "Add with infixed symbol"))

; Strategy: Data decomposition on a : Add

(define (add->sExpr a)
  (if (add-infix? a)
      (exprNelist->sExpr-infix (add-exprs a) PLUS)
      (exprNelist->sExpr-prefix (add-exprs a) PLUS)))

; Tests

(begin-for-test
  (check-equal? (add->sExpr 
                 (make-add (list 3 (make-add (list 5 4) true)) false))
                (list "+" "3" (list "5" "+" "4"))
                "returns a SExpr that represents the given Add"))

;------------------------------------------------------------------------------

; mul->sExpr : Mul -> SExpr
; Returns a rendering of m as an SExpr

; Examples :

(begin-for-test
  (check-equal? (mul->sExpr (make-mul (list 3 4) false))
                (list "*" "3" "4")
                "Mul with prefixed symbol")
  (check-equal? (mul->sExpr (make-mul (list 5 6 7 8) true))
                (list "5" "*" "6" "*" "7" "*" "8")
                "Mul with infixed symbol"))

; Strategy: Data decomposition on m : Mul

(define (mul->sExpr m)
  (if (mul-infix? m)
      (exprNelist->sExpr-infix (mul-exprs m) MULT)
      (exprNelist->sExpr-prefix (mul-exprs m) MULT)))

; Tests

(begin-for-test
  (check-equal? (mul->sExpr 
                 (make-mul (list 3 (make-mul (list 5 4) true)) false))
                (list "*" "3" (list "5" "*" "4"))
                "returns a SExpr that represents the given Mul"))

;------------------------------------------------------------------------------

; exprNelist->sExpr-infix : ExprNelist OptrString -> SExpr
; Returns a rendering of nel as a SExpr with op infixed

; Examples:

(begin-for-test
  (check-equal? (exprNelist->sExpr-infix (list 2 3 4) MULT)
                (list "2" "*" "3" "*" "4")
                "Only numbers")
  (check-equal? (exprNelist->sExpr-infix 
                 (list 2 3 (make-add (list 3 4) false)) PLUS)
                (list "2" "+" "3" "+" (list "+" "3" "4"))
                "Numbers and Add"))

; Strategy: Data decomposition on nel: ExprNelist

(define (exprNelist->sExpr-infix nel op)
    (append (list (expr->sExpr (first nel)))  
            (foldr (λ (ex b) (append (list op (expr->sExpr ex)) b))
                   EMPTY-LS 
                   (rest nel))))

;------------------------------------------------------------------------------

; exprNelist->sExpr-prefix : ExprNelist OptrString -> ExprStrList
; Returns a rendering of nel as a SExpr with op prefixed

; Examples:

(begin-for-test
  (check-equal? (exprNelist->sExpr-prefix (list 2 3 4) PLUS)
                (list "+" "2" "3" "4")
                "Only numbers")
  (check-equal? (exprNelist->sExpr-prefix 
                 (list 2 3 (make-mul (list 3 4) true)) MULT)
                (list "*" "2" "3" (list "3" "*" "4"))
                "Numbers and Mul"))

; Strategy: Data decomposition on nel: ExprNelist

(define (exprNelist->sExpr-prefix nel op)
  (append (list op (expr->sExpr (first nel)))  
          (map expr->sExpr (rest nel))))

;------------------------------------------------------------------------------

