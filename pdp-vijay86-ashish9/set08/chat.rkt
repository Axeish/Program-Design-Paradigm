;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 5)
(require 2htdp/universe)
(require 2htdp/image)
(require racket/string)

;; Provide Functions
;;----------------------------------------------------------------------------

(provide mk-world)
(provide receive)
(provide key-handler)
(provide get-users)
(provide get-editor)
(provide get-editor-pre)
(provide get-editor-post)
(provide get-chat-history)

;; Constants
;;------------------------------------------------------------------------------

(define EDITOR-BODY (empty-scene 300 20));Image
(define EDITOR-BODY-HEIGHT (image-height EDITOR-BODY));NonNegReal
(define FONT-SIZE 12)
(define COLOR "black"); Color
(define CURSOR (rectangle 2 18 "solid" "red"));Image
(define CURSOR-WIDTH (image-width CURSOR)) ;NonNegReal
(define LEFT "left");KeyEvent
(define RIGHT "right");KeyEvent
(define BACK-SPACE "\b");KeyEvent
(define TAB "\t");KeyEvent
(define RUBOUT "\u007F");KeyEvent
(define ENTER "\r");KeyEvent


(define R "r");KeyEvent
(define O "o");KeyEvent
(define DOWN "down"); KeyEvent

(define MT-SCENE (empty-scene 400 400)) ;Image
(define CANVAS-HEIGHT (image-height MT-SCENE)) ;NonNegReal
(define NAMES-AREA (empty-scene 100 400)) ;Image
(define CHAT-AREA (empty-scene 300 380));image
(define NAMES-AREA-WIDTH (image-width NAMES-AREA));NonNegReal
(define CHAT-TXT-WIDTH (- (image-width CHAT-AREA) 
                          (image-width CURSOR))) ;NonNegReal
(define TEXT-HEIGHT (image-height (text "A" FONT-SIZE COLOR))) ;NonNegReal
(define TEST-NAMES-AREA (empty-scene 100 TEXT-HEIGHT)) ;Image
(define START-Y (/ TEXT-HEIGHT 2)) ;Coordinate

(define MT-STRING "") ;String
(define PVT-CLR "blue") ;Color
(define JL-CLR "gray") ;Color
(define ERROR-CLR "red") ;Color
(define MT-LIST '()) ;empty list
(define SOME-STRING "some string") ;String
(define LONG-STRING "@@@@@@@@@@@@@@@@") ;String
(define HALF-LONG-STRING "@@@@@@@@");String
(define VERY-LONG-STRING "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
(define HALF-LONG-TEXT (text HALF-LONG-STRING FONT-SIZE COLOR)) ;Image

(define LO1S-HELL (list "h" "e" "l" "l")) ;ListOf<1String>
(define LO1S-O (list "o"))  ;ListOf<1String>
(define LO1S-EMPTY '()) ;ListOf<1String>
(define LO1S-HELLO (list "h" "e" "l" "l" "o")) ;ListOf<1String>
(define LO1S-WOR (list "w" "o" "r")) ;ListOf<1String>
(define LO1S-WORR (list "w" "o" "r" "r")) ;ListOf<1String>
(define LO1S-LD (list "l" "d")) ;ListOf<1String>
(define LO1S-WO (list "w" "o")) ;ListOf<1String>
(define LO1S-RLD (list "r" "l" "d")) ;ListOf<1String>

(define R-LO1S-HELLO (reverse LO1S-HELLO)) ;ListOf<1String>
(define R-LO1S-HELL (reverse LO1S-HELL)) ;ListOf<1String>
(define R-LO1S-WOR (reverse LO1S-WOR)) ;ListOf<1String>
(define R-LO1S-WORR (reverse LO1S-WORR)) ;ListOf<1String>
(define R-LO1S-WO (reverse LO1S-WO)) ;ListOf<1String>


(define TEXT-HELL (text "hell" FONT-SIZE COLOR)) ;Image
(define TEXT-HELLO (text "hello" FONT-SIZE COLOR)) ;Image
(define TEXT-O (text "o" FONT-SIZE COLOR)) ;Image
(define TEXT-EMPTY (text "" FONT-SIZE COLOR)) ;Image
(define TEXT-PDP (text "pdp" FONT-SIZE COLOR)) ;Image
(define STR-HELLO "hello") ;String

(define CHAT-WIDTH (image-width CHAT-AREA)) ;NonNegReal
(define CHAT-HEIGHT (image-height CHAT-AREA)) ;NonNegReal

(define EXAMPLE-CHAT-HISTORY (list SOME-STRING SOME-STRING SOME-STRING 
                                   SOME-STRING)) ;ListOf<String>

(define ZERO 0) ;Integer
(define ONE 1) ;Integer
(define TWO 2) ;Integer
(define MAX-USER-LEN 12) ;NonNegInteger
(define MAX-CHAT-HISTORY 25) ;NonNegInteger
(define SPACE " ") ;String

(define LEFT-ALIGN "left") ; X-place
(define MIDDLE-ALIGN "middle") ; Y-place

(define JOIN-POST-STR " joined.") ;String
(define LEAVE-POST-STR " left the chat.") ;String
(define OPEN-BRACKET "< ") ;String
(define CLOSE-BRACKET " > ") ;String
(define ARROW "->") ;String
(define COLON ":") ;1String

;; Data Definitions
;;------------------------------------------------------------------------------

;; A UserName is a String, consisting of only letters and numbers,
;; and is no longer than 12 characters.
;; Represents a chat room participant.

;; Data Example:

(define USERNAME1 "chatMaster")
(define USERNAME2 "chatBuddy")
(define USERNAME3 "a1")

;;------------------------------------------------------------------------------

;; Template for ListOf<UserName>
;; username-ls-fn : ListOf<UserName> -> ???
;; (define (username-ls-fn ls)
;;   (cond
;;     [(empty? ls) ...]
;;     [else (...(first ls)...(username-ls-fn (rest ls)))]))

;;------------------------------------------------------------------------------

(define USERLIST (list USERNAME1 USERNAME2))
(define SORTED-USERLIST (list USERNAME2 USERNAME1))
(define USERNAME1-TEXT (text USERNAME1 FONT-SIZE COLOR))
(define USERNAME2-TEXT (text USERNAME2 FONT-SIZE COLOR))

;;------------------------------------------------------------------------------

;; A Message is a String

;; Data example:

(define MESSAGE "Hello how are you?")

;;------------------------------------------------------------------------------

;; MsgToServer is a:
;; - (list 'broadcast Message)
;; - (list 'private UserName Message)
;; The user in the private message is the intended recipient of the message.

;; Data Examples:

(define TO-SERVER-BROADCAST (list 'broadcast MESSAGE))
(define TO-SERVER-PRIVATE (list 'private USERNAME1 MESSAGE))

;; <MsgToServer predicates> : MsgToServer -> Boolean
;; Returns true if msg is the MsgToServer indicated by the function name.
;; STRATEGY: Data decomposition on msg: MsgToServer
(define (broadcast-to? msg) (symbol=? (first msg) 'broadcast))
(define (private-to? msg) (symbol=? (first msg) 'private))

;; Template
;; msgToServer-fn: MsgToServer -> ???
;; (define (msgToServer-fn msg)
;;   (cond
;;     [(broadcast-to? msg) (...(first msg)...(second msg)...)]
;;     [(private-to? msg) (...(first msg)...(second msg)...(third msg)...)]))

;;------------------------------------------------------------------------------

;; A MsgFromServer is a:
;; - (list 'userlist ListOf<UserName>) ; all the current chat participants
;; - (list 'join UserName) ; the user that just joined
;; - (list 'leave UserName) ; the user that just left
;; - (list 'error Message) ; an error message from the server
;; - (list 'private UserName Message) ; a priv msg from the specified user
;; - (list 'broadcast UserName Message) ; broadcast msg from the specified user

;; Interpretation: Represents a message sent from the server

;; Data Examples:

(define FROM-SERVER-USERLIST (list 'userlist USERLIST))
(define FROM-SERVER-JOIN (list 'join USERNAME1))
(define FROM-SERVER-LEAVE (list 'leave USERNAME1))
(define FROM-SERVER-ERROR (list 'error "user does not exist"))
(define FROM-SERVER-PRIVATE (list 'private USERNAME1 MESSAGE))
(define FROM-SERVER-BROADCAST (list 'broadcast USERNAME2 MESSAGE))

;; <MsgFromServer predicates> : MsgFromServer -> Boolean
;; Returns true if msg is the MsgFromServer indicated by the function name.
;; STRATEGY: Data decomposition on msg: MsgFromServer
(define (userlist? msg) (symbol=? (first msg) 'userlist))
(define (join? msg) (symbol=? (first msg) 'join))
(define (leave? msg) (symbol=? (first msg) 'leave))
(define (error? msg) (symbol=? (first msg) 'error))
(define (private-from? msg) (symbol=? (first msg) 'private))
(define (broadcast-from? msg) (symbol=? (first msg) 'broadcast))

;; Template:
;; msgFromServer-fn : MsgFromServer -> ???
;; (define (msgFromServer-fn msg)
;;   (cond
;;     [(userlist? msg) (...(first msg)...(username-ls-fn (second msg))...)]
;;     [(join? msg) (...(first msg)...(username-fn (second msg))...)]
;;     [(leave? msg) (...(first msg)...(username-fn (second msg))...)]
;;     [(error? msg) (...(first msg)...(message-fn (second msg))...)]
;;     [(private-from? msg) (...(first msg)...(username-fn (second msg))
;;                           ...(message-fn (third msg))...)]
;;     [(broadcast-from? msg) (...(first msg)...(username-fn (second msg))
;;                             ...(message-fn (third msg))...)]))

;;------------------------------------------------------------------------------

;; A MsgForClient is a:
;; - (list 'join String) ; string informing that a user joined
;; - (list 'leave String) ; string informing that a user left
;; - (list 'error String) ; an error message from the server
;; - (list 'private String) ; string with name and private message from a user
;; - (list 'broadcast String) ; string with name and message from a user

;; Interpretation: Represents a message to be displayed in the client

;; Data Examples:

(define CLIENT-JOIN (list 'join SOME-STRING))
(define CLIENT-LEAVE (list 'leave SOME-STRING))
(define CLIENT-ERROR (list 'error SOME-STRING))
(define CLIENT-PRIVATE (list 'private SOME-STRING))
(define CLIENT-BROADCAST (list 'broadcast SOME-STRING))
(define CLIENT-BROADCAST-LONG (list 'broadcast LONG-STRING))

;; MsgForClient predicates> : MsgForClient -> Boolean
;; Returns true if msg is the MsgForClient indicated by the function name.
;; STRATEGY: Data decomposition on msg: MsgForClient
(define (join-mc? msg) (symbol=? (first msg) 'join))
(define (leave-mc? msg) (symbol=? (first msg) 'leave))
(define (error-mc? msg) (symbol=? (first msg) 'error))
(define (private-mc? msg) (symbol=? (first msg) 'private))
(define (broadcast-mc? msg) (symbol=? (first msg) 'broadcast))

;; Template:
;; msgForClient-fn : MsgForClient -> ???
;; (define (msgForClient-fn msg)
;;   (cond
;;     [(join-mc? msg) (...(first msg)...(second msg)...)]
;;     [(leave-mc? msg) (...(first msg)...(second msg)...)]
;;     [(error-mc? msg) (...(first msg)...(second msg)...)]
;;     [(private-mc? msg) (...(first msg)...(second msg)...)]
;;     [(broadcast-mc? msg) (...(first msg)...(second msg)...)]))

;;------------------------------------------------------------------------------

;; Template for ListOf<MsgForClient>:
;; msgForClient-ls-fn : ListOf<MsgForClient> -> ???
;; (define (msgForClient-ls-fn ls)
;;   (cond
;;     [(empty? ls) ...]
;;     [else (... (first (first ls))...(second (first ls)
;;            ... (msgForClient-ls-fn (rest ls)))]))

;;------------------------------------------------------------------------------

(define CHAT-LIST (list CLIENT-JOIN CLIENT-ERROR CLIENT-PRIVATE 
                        CLIENT-BROADCAST))
(define CHAT-LIST-FULL (list CLIENT-JOIN CLIENT-ERROR CLIENT-PRIVATE 
                             CLIENT-BROADCAST CLIENT-ERROR CLIENT-ERROR
                             CLIENT-LEAVE CLIENT-BROADCAST CLIENT-BROADCAST
                             CLIENT-BROADCAST CLIENT-BROADCAST CLIENT-BROADCAST
                             CLIENT-BROADCAST CLIENT-BROADCAST CLIENT-BROADCAST
                             CLIENT-LEAVE CLIENT-BROADCAST CLIENT-BROADCAST
                             CLIENT-BROADCAST CLIENT-BROADCAST CLIENT-BROADCAST
                             CLIENT-BROADCAST CLIENT-BROADCAST CLIENT-BROADCAST
                             CLIENT-ERROR))
(define CHAT-LIST-LONG (list CLIENT-BROADCAST-LONG))

;;------------------------------------------------------------------------------

;; An Editor is a (make-editor ListOf<1String> ListOf<1String> NonNegReal) 
(define-struct editor [before after plen])

;; Interpretation : (make-editor s t len) means (implode s) is the reversed form
;; of the string before the cursor, (implode t) is the string after the cursor 
;; and len is the minimum display length assigned to (implode t) while rendering

;; DATA EXAMPLES:

(define EDITOR-HELL-O (make-editor (list "l" "l" "e" "h") LO1S-O ZERO))
(define EDITOR-PRIVATE (make-editor (list "o" "l" "l" "e" "h" ":" "1" "a")
                                    LO1S-EMPTY ZERO))
(define EDITOR-EMPTY (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
(define EDITOR-C (make-editor (list "c") empty ZERO))
(define EDITOR-CHATBUDDY (make-editor (list "y" "d" "d" "u" "B" "t" "a" "h" "c")
                                      empty ZERO))
(define EDITOR-FULL (make-editor (explode VERY-LONG-STRING) 
                                 (explode VERY-LONG-STRING)
                                 ZERO))

;; Template:
;; editor-fn : Editor -> ???
;; (define (editor-fn e)
;;  (... (lo1S-fn (editor-before e)) ... (lo1S-fn (editor-after e)) ...))

;;------------------------------------------------------------------------------

;; A World is (make-world Editor ListOf<UserName> ListOf<MsgForClient> UserName)
(define-struct world [editor users chat name])

;; Interpretation : Represents the current state of the chat client

;; Data Examples:

(define EXAMPLE-WORLD (make-world EDITOR-HELL-O USERLIST CHAT-LIST USERNAME1))
(define PRIVATE-WORLD (make-world EDITOR-PRIVATE USERLIST CHAT-LIST USERNAME1))

;; Template
;; (define (world-fn w)
;;   (...(editor-fn (world-editor w))...
;;       ...(username-ls-fn (world-users w))...
;;       ...(msgForClient-ls-fn (world-chat w))...
;;       ...(world-name w)...))

;;------------------------------------------------------------------------------

;; A HandlerResult is a:
;; - World
;; - (make-package World MsgToServer)

;; Data Examples:

(define HANDLER-WORLD EXAMPLE-WORLD)
(define HANDLER-WORLD-PKG (make-package EXAMPLE-WORLD TO-SERVER-BROADCAST))

;; Template
;; handlerResult-fn : HandlerResult -> ???
;; (define (handlerResult-fn hr)
;;   (cond
;;     [(not (package? hr)) (...(world-fn hr)...)]
;;     [else (...(package-fn hr))]))

;;------------------------------------------------------------------------------

;; TEMPLATE:
;; nelist-fn : NEListOf<X> -> ???
;; (define (nelist-fn nelst)
;;   (... (X-fn (first nelst)) ...
;;        (list-fn (rest nelst)) ...))

;;------------------------------------------------------------------------------

;; FUNCTIONS

;;------------------------------------------------------------------------------

;; mk-world : UserName -> World
;; Returns the initial world state for user name.

;; Examples:

(begin-for-test
  (check-equal? (mk-world USERNAME1)
                (make-world EDITOR-EMPTY MT-LIST MT-LIST USERNAME1)
                "world created with USERNAME1"))

;; Strategy: Function composition

(define (mk-world nam) 
  (make-world EDITOR-EMPTY MT-LIST MT-LIST nam))

;;------------------------------------------------------------------------------

;; receive : World MsgFromServer -> HandlerResult
;; Handles messages received from the server.

;; Examples:

(begin-for-test 
  (check-equal?  (receive EXAMPLE-WORLD FROM-SERVER-USERLIST)
                 (make-world (world-editor EXAMPLE-WORLD) 
                             SORTED-USERLIST
                             (world-chat EXAMPLE-WORLD) 
                             (world-name EXAMPLE-WORLD))
                 "Sorted userlist is stored in the world"))

;; Strategy: Data decomposition on msg: MsgFromServer

(define (receive w msg)
  (cond
    [(userlist? msg) (make-world (world-editor w) 
                                 (sort (second msg) string-ci-sf<?) 
                                 (world-chat w) 
                                 (world-name w))]
    [(join? msg) (make-world (world-editor w) 
                             (add-user (second msg) (world-users w)) 
                             (add-to-chat msg (world-chat w))
                             (world-name w))]
    [(leave? msg) (make-world (world-editor w) 
                              (remove (second msg) (world-users w)) 
                              (add-to-chat msg (world-chat w))
                              (world-name w))]
    [(error? msg) (make-world (world-editor w) 
                              (world-users w) 
                              (add-to-chat msg (world-chat w))
                              (world-name w))]
    [(private-from? msg) (make-world (world-editor w) 
                                     (world-users w) 
                                     (add-to-chat msg (world-chat w))
                                     (world-name w))]
    [(broadcast-from? msg) (make-world (world-editor w) 
                                       (world-users w) 
                                       (add-to-chat msg (world-chat w))
                                       (world-name w))]))

;; Tests:

(begin-for-test 
  (check-equal?  (receive EXAMPLE-WORLD FROM-SERVER-JOIN)
                 (make-world (world-editor EXAMPLE-WORLD) 
                             (add-user (second FROM-SERVER-JOIN) 
                                       (world-users EXAMPLE-WORLD))
                             (add-to-chat FROM-SERVER-JOIN 
                                          (world-chat EXAMPLE-WORLD)) 
                             (world-name EXAMPLE-WORLD))
                 "Adds user to user list and adds a message to the chat list")
  (check-equal?  (receive EXAMPLE-WORLD FROM-SERVER-LEAVE)
                 (make-world (world-editor EXAMPLE-WORLD) 
                             (remove (second FROM-SERVER-LEAVE) 
                                     (world-users EXAMPLE-WORLD))
                             (add-to-chat FROM-SERVER-LEAVE 
                                          (world-chat EXAMPLE-WORLD)) 
                             (world-name EXAMPLE-WORLD))
                 "Removes user from user list and adds a message to chat")
  (check-equal?  (receive EXAMPLE-WORLD FROM-SERVER-ERROR)
                 (make-world (world-editor EXAMPLE-WORLD) 
                             (world-users EXAMPLE-WORLD)
                             (add-to-chat FROM-SERVER-ERROR 
                                          (world-chat EXAMPLE-WORLD)) 
                             (world-name EXAMPLE-WORLD))
                 "Adds error message to chat")
  (check-equal?  (receive EXAMPLE-WORLD FROM-SERVER-PRIVATE)
                 (make-world (world-editor EXAMPLE-WORLD) 
                             (world-users EXAMPLE-WORLD)
                             (add-to-chat FROM-SERVER-PRIVATE 
                                          (world-chat EXAMPLE-WORLD)) 
                             (world-name EXAMPLE-WORLD))
                 "Private message")
  (check-equal?  (receive EXAMPLE-WORLD FROM-SERVER-BROADCAST)
                 (make-world (world-editor EXAMPLE-WORLD) 
                             (world-users EXAMPLE-WORLD)
                             (add-to-chat FROM-SERVER-BROADCAST 
                                          (world-chat EXAMPLE-WORLD)) 
                             (world-name EXAMPLE-WORLD))
                 "Broadcast message"))

;;------------------------------------------------------------------------------

;; key-handler : World KeyEvent -> HandlerResult
;; Handles keyboard user input.

;; Examples:

(begin-for-test
  (check-equal? (key-handler EXAMPLE-WORLD LEFT)
                (key-left-world EXAMPLE-WORLD)
                "editor's cursor moved left"))

;; Strategy: Data decomposition on ke: KeyEvent

(define (key-handler w ke) 
  (cond
    [(key=? ke LEFT)  (key-left-world w)]
    [(key=? ke RIGHT) (key-right-world w)]
    [(key=? BACK-SPACE ke) (key-back-world w)]
    [(key=? ENTER ke) (key-enter w)]
    [(key=? ke TAB) (key-tab-world w)]
    [(key=? ke RUBOUT) w]
    [(= (string-length ke) 1) (key-single-world w ke)]
    [else w]))

;; Tests:

(begin-for-test
  (check-equal? (key-handler EXAMPLE-WORLD RIGHT)
                (key-right-world EXAMPLE-WORLD)
                "editor's cursor moved right")
  (check-equal? (key-handler EXAMPLE-WORLD BACK-SPACE)
                (key-back-world EXAMPLE-WORLD)
                "the character to the left of the cursor deleted")
  (check-equal? (key-handler EXAMPLE-WORLD ENTER)
                (key-enter EXAMPLE-WORLD)
                "send's message to server")
  (check-equal? (key-handler EXAMPLE-WORLD TAB)
                (key-tab-world EXAMPLE-WORLD)
                "autocomplete username")
  (check-equal? (key-handler EXAMPLE-WORLD R)
                (key-single-world EXAMPLE-WORLD R)
                "handles a single character input")
  (check-equal? (key-handler EXAMPLE-WORLD RUBOUT)
                EXAMPLE-WORLD
                "rubout is ignored")
  (check-equal? (key-handler EXAMPLE-WORLD DOWN)
                EXAMPLE-WORLD
                "All other keys are ignored. 'down' key should be ignored and 
                 World returned as is"))

;;------------------------------------------------------------------------------

;; key-left-world : World -> World
;; moves editor cursor left

;; Examples:

(begin-for-test
  (check-equal? (key-left-world EXAMPLE-WORLD)
                (make-world (make-editor (list "l" "e" "h") (list "l" "o") ZERO)
                            (world-users EXAMPLE-WORLD) 
                            (world-chat EXAMPLE-WORLD) 
                            (world-name EXAMPLE-WORLD))
                "editor's cursor moved left"))

;; Strategy: Data decomposition on w: World

(define (key-left-world w)
  (make-world (key-left (world-editor w)) 
              (world-users w)
              (world-chat w)
              (world-name w)))

;;------------------------------------------------------------------------------

;; key-right-world : World -> World
;; moves editor cursor right

;; Examples:

(begin-for-test
  (check-equal? (key-right-world EXAMPLE-WORLD)
                (make-world (make-editor (list "o" "l" "l" "e" "h") empty ZERO) 
                            (world-users EXAMPLE-WORLD) 
                            (world-chat EXAMPLE-WORLD) 
                            (world-name EXAMPLE-WORLD))
                "editor's cursor moved right"))

;; Strategy: Data decomposition on w: World

(define (key-right-world w)
  (make-world (key-right (world-editor w)) 
              (world-users w)
              (world-chat w)
              (world-name w)))

;;------------------------------------------------------------------------------

;; key-back-world : World -> World
;; deletes the character to the left of the cursor

;; Examples:

(begin-for-test
  (check-equal? (key-back-world EXAMPLE-WORLD)
                (make-world (make-editor (list "l" "e" "h") (list "o") ZERO) 
                            (world-users EXAMPLE-WORLD) 
                            (world-chat EXAMPLE-WORLD) 
                            (world-name EXAMPLE-WORLD))
                "deletes the character to the left of the cursor"))

;; Strategy: Data decomposition on w: World

(define (key-back-world w)
  (make-world (key-back (world-editor w)) 
              (world-users w)
              (world-chat w)
              (world-name w)))

;;------------------------------------------------------------------------------

;; key-single-world : World KeyEvent -> World
;; adds k to the left of the cursor

;; Examples:

(begin-for-test
  (check-equal? (key-single-world EXAMPLE-WORLD R)
                (make-world (make-editor (list "r" "l" "l" "e" "h") (list "o")
                                         ZERO) 
                            (world-users EXAMPLE-WORLD) 
                            (world-chat EXAMPLE-WORLD) 
                            (world-name EXAMPLE-WORLD))
                "adds 'r' to the left of the cursor"))

;; Strategy: Data decomposition on w: World

(define (key-single-world w k)
  (make-world (key-single (world-editor w) k) 
              (world-users w)
              (world-chat w)
              (world-name w)))

;;------------------------------------------------------------------------------

;; get-editor-str: Editor -> String
;; returns the text in the editor

;; Examples:

(begin-for-test
  (check-equal? (get-editor-str EDITOR-EMPTY)
                MT-STRING
                "returns string in the editor"))

;; Strategy: Function composition

(define (get-editor-str e)
  (string-append (get-editor-pre e) (get-editor-post e)))

;; Tests:

(begin-for-test
  (check-equal? (get-editor-str EDITOR-HELL-O)
                "hello"
                "returns string in the editor"))

;;------------------------------------------------------------------------------

;; is-alpha-numeric?: String -> Boolean
;; Checks if the string str only contains letters from the alphabet or numbers

;; Examples:

(begin-for-test
  (check-equal? (is-alpha-numeric? "String123")
                true
                "String123 is alphanumeric"))

;; Strategy: Function composition

(define (is-alpha-numeric? str)
  (local (; is-alpha-or-num?: 1String -> Boolean
          ; Checks if the 1string s is a letter from the alphabet or a number
          ; Strategy: Function composition
          (define (is-alpha-or-num? s)
            (or (string-alphabetic? s) (string-numeric? s))))
    ;--IN--
    (andmap is-alpha-or-num? (explode str))))

;; Tests:

(begin-for-test
  (check-equal? (is-alpha-numeric? "String")
                true
                "Only contains letters from the alphabet")
  (check-equal? (is-alpha-numeric? "1234")
                true
                "Only contains numbers")
  (check-equal? (is-alpha-numeric? "1234 String")
                false
                "contains space")
  (check-equal? (is-alpha-numeric? "1234@String")
                false
                "contains @"))

;;------------------------------------------------------------------------------

;; str->msg-to-server: String -> MsgToServer
;; converts given string to a MsgToServer. Returns a private MsgToServer if the
;; string before ':' is a valid username, else returns a broadcast MsgToServer
;; Where: str contains ':'

;; Examples:

(begin-for-test
  (check-equal? (str->msg-to-server "ChatMan: Hello")
                (list 'private "ChatMan" " Hello")
                "private message"))

;; Strategy: Function composition

(define (str->msg-to-server str)
  (local (; index of ':' in str
          (define index (index-of COLON str))
          ; string before ':'
          (define user (substring str ZERO index))
          ; length of the string before ':'
          (define len (string-length user))
          ; checking if user is alphanumeric
          (define is-alp-num? (is-alpha-numeric? user))
          ; string after ':'
          (define msg (substring str (add1 index))))
    ;--IN--
    (if (and (<= len MAX-USER-LEN) is-alp-num?)
        (list 'private user msg)
        (list 'broadcast str))))

;; Tests:

(begin-for-test
  (check-equal? (str->msg-to-server "ChatMan@@: Hello")
                (list 'broadcast "ChatMan@@: Hello")
                "not a valid username")
  (check-equal? (str->msg-to-server "ChatManChatMan: Hello")
                (list 'broadcast "ChatManChatMan: Hello")
                "too long to be a user name"))

;;------------------------------------------------------------------------------

;; key-enter: World -> HandlerResult
;; sends the text in the editor to the server as a private message or broadcast

;; Examples:

(begin-for-test
  (check-equal? (key-enter EXAMPLE-WORLD)
                (make-package (make-world EDITOR-EMPTY 
                                          (world-users EXAMPLE-WORLD)
                                          (add-my-msg-to-chat 
                                           (world-name EXAMPLE-WORLD) 
                                           (list 'broadcast STR-HELLO)
                                           (world-chat EXAMPLE-WORLD))
                                          (world-name EXAMPLE-WORLD)) 
                              (list 'broadcast STR-HELLO))
                "broadcast message"))

;; Strategy: Data decomposition on w: World

(define (key-enter w)
  (local (; string currently inside editor
          (define editor-str (get-editor-str (world-editor w)))
          ; MsgToServer from the string inside editor
          (define msg (if (string-contains? COLON editor-str)
                          (str->msg-to-server editor-str)
                          (list 'broadcast editor-str)))
          ; updated world
          (define new-w (make-world EDITOR-EMPTY 
                                    (world-users w)
                                    (add-my-msg-to-chat (world-name w) 
                                                        msg 
                                                        (world-chat w))
                                    (world-name w))))
    ;--IN--
    (make-package new-w msg)))

;; Tests:

(begin-for-test
  (check-equal? (key-enter PRIVATE-WORLD)
                (make-package (make-world EDITOR-EMPTY 
                                          (world-users PRIVATE-WORLD)
                                          (add-my-msg-to-chat 
                                           (world-name PRIVATE-WORLD) 
                                           (list 'private USERNAME3 STR-HELLO)
                                           (world-chat PRIVATE-WORLD))
                                          (world-name PRIVATE-WORLD)) 
                              (list 'private USERNAME3 STR-HELLO))
                "private message"))

;;------------------------------------------------------------------------------

;; key-tab-world: World -> World
;; autocompletes username if the string in the editor is part of a valid an
;; existing username

;; Example:

(begin-for-test
  (check-equal? (key-tab-world (make-world EDITOR-C 
                                           SORTED-USERLIST 
                                           CHAT-LIST 
                                           USERNAME1))
                (make-world EDITOR-CHATBUDDY 
                            SORTED-USERLIST 
                            CHAT-LIST 
                            USERNAME1)
                "username autocompleted"))

;; Strategy: Data decomposition on w: World

(define (key-tab-world w)
  (make-world (key-tab (world-editor w) (world-users w)) 
              (world-users w)
              (world-chat w)
              (world-name w)))

;;------------------------------------------------------------------------------

;; is-valid-user-and-non-empty? : Editor -> Boolean
;; checks if the string before the cursor is a valid UserName and is non empty.
;; Returns false if  there are characters after the cursor

;; Examples:

(begin-for-test
  (check-equal? (is-valid-user-and-non-empty? EDITOR-C)
                true
                "valid username"))

;; Strategy: function composition

(define (is-valid-user-and-non-empty? e)
  (and (string=? (get-editor-post e) MT-STRING)
           (not (string=? (get-editor-pre e) MT-STRING))
           (< (string-length (get-editor-pre e)) MAX-USER-LEN)))


;; Tests:

(begin-for-test
  (check-equal? (is-valid-user-and-non-empty? EDITOR-HELL-O)
                false
                "there are characters after the cursor")
  (check-equal? (is-valid-user-and-non-empty? EDITOR-EMPTY)
                false
                "editor is empty"))


;;------------------------------------------------------------------------------

;; key-tab: Editor ListOf<UserName> -> Editor
;; autocompletes username if the string in the editor is part of the list users

;; Examples

(begin-for-test
  (check-equal? (key-tab EDITOR-C SORTED-USERLIST)
                EDITOR-CHATBUDDY
                "username autocompleted")
  (check-equal? (key-tab EDITOR-EMPTY SORTED-USERLIST)
                EDITOR-EMPTY
                "nothing to autocomplete"))

;; Strategy: Function composition

(define (key-tab e users)
  (if (is-valid-user-and-non-empty? e)
      (make-editor (reverse (explode (find-name (get-editor-pre e) users)))
                   MT-LIST ZERO)
      e))

;;------------------------------------------------------------------------------

;; render: World -> Image
;; Draws a representation of the current state of the World w

;; Examples

(begin-for-test
  (check-equal? (render EXAMPLE-WORLD)
                (get-canvas (print-user-names USERLIST NAMES-AREA)
                            (print-msgs (reverse CHAT-LIST) CHAT-AREA)
                            (draw-editor EDITOR-HELL-O))
                "current state of the world"))

;; Strategy: Data decomposition on w: World

(define (render w)
  (local (; current editor image
          (define editor (draw-editor (world-editor w)))
          ; current user names area image
          (define names-area (print-user-names (world-users w) NAMES-AREA))
          ; current chat area image
          (define chat-area (print-msgs (msg-list-that-fits (world-chat w)
                                                            CHAT-WIDTH
                                                            CHAT-HEIGHT)
                                        CHAT-AREA)))
    ;--IN--
    (get-canvas names-area chat-area editor)))

;;------------------------------------------------------------------------------

;; get-users : World -> ListOf<UserName>
;; Returns a list of current chat participants, in lexicographic order.

;; Examples:

(begin-for-test
  (check-equal? (get-users EXAMPLE-WORLD)
                SORTED-USERLIST
                "sorted userlist"))

;; Strategy: Data decomposition on w: World

(define (get-users w) 
  (sort (world-users w) string-ci-sf<?))

;;------------------------------------------------------------------------------

;; get-editor : World -> Editor
;; Returns a representation of the chat client's input area.

;; Examples:

(begin-for-test
  (check-equal? (get-editor EXAMPLE-WORLD)
                EDITOR-HELL-O
                "current editor"))

;; Strategy: Data decomposition on w: World

(define (get-editor w) 
  (world-editor w))

;;------------------------------------------------------------------------------

;; get-chat-history : World -> ListOf<String>
;; Returns a list of chat events, rendered to string, 
;; where each string format is the same as when the event is
;; rendered to the chat window.

;; Examples:

(begin-for-test
  (check-equal? (get-chat-history EXAMPLE-WORLD)
                EXAMPLE-CHAT-HISTORY
                "current chat history"))

;; Strategy: Data decomposition on w: World

(define (get-chat-history w) 
  (map second (world-chat w)))

;;------------------------------------------------------------------------------

;; run : UserName IPAddress -> World
;; Connect to the given chat server with user name nam.
(define (run nam server)
  (big-bang (mk-world nam)
            (on-receive receive)
            (to-draw render)
            (on-key key-handler)
            (name nam)
            (register server)
            (port 5010)))

;;------------------------------------------------------------------------------

;; get-canvas: Image Image Image -> Image
;; Puts together different parts of the chat client into a single image

;; Example:

(begin-for-test
  (check-equal? (get-canvas empty-image empty-image empty-image)
                MT-SCENE
                "nothing to place"))

;; Strategy: Function composition

(define (get-canvas names-area chat-area editor-image)
  (place-image editor-image
               (+ NAMES-AREA-WIDTH (/ CHAT-WIDTH TWO))
               (- CANVAS-HEIGHT (/ EDITOR-BODY-HEIGHT TWO))
               (place-image chat-area 
                            (+ NAMES-AREA-WIDTH (/ CHAT-WIDTH TWO))
                            (/ (- CANVAS-HEIGHT EDITOR-BODY-HEIGHT) TWO)
                            (place-image names-area 
                                         (/ NAMES-AREA-WIDTH TWO) 
                                         (/ CANVAS-HEIGHT TWO) 
                                         MT-SCENE))))

;;------------------------------------------------------------------------------

;; mt-checked-rest-list : ListOf<1String> -> ListOf<1String>
;; Given the ListOf<1String> l, removes the first element and returns the rest.
;; If l is empty, it returns an empty ListOf<1String>

(begin-for-test 
  (check-equal?  (mt-checked-rest-list (list "p" "o" "t"))
                 (list "o" "t")
                 "l is returned after removing the first element")
  (check-equal?  (mt-checked-rest-list (list "i"))
                 empty
                 "Since input length is 1, returns an empty list")
  (check-equal?  (mt-checked-rest-list empty)
                 empty
                 "Since input is empty, returns an empty list"))

;; Strategy: Data decomposition on l : ListOf<1String>

(define (mt-checked-rest-list l)
  (cond
    [(empty? l) MT-LIST]
    [else (rest l)]))

;;------------------------------------------------------------------------------

;; mt-checked-first-list : ListOf<1String> -> ListOf<1String>
;; Given l, returns a ListOf<1String> with just the first element. If l is empty
;; it returns an empty ListOf<1String>

(begin-for-test 
  (check-equal?  (mt-checked-first-list (list "p" "o" "t"))
                 (list "p")
                 "l is returned after removing the first element")
  (check-equal?  (mt-checked-first-list (list "i"))
                 (list "i")
                 "Since input length is 1, returns the same list")
  (check-equal?  (mt-checked-first-list empty)
                 empty
                 "Since input is empty, returns an empty LO1S"))

;; Strategy: Data decomposition on l : ListOf<1String>

(define (mt-checked-first-list l)
  (cond
    [(empty? l) MT-LIST]
    [else (cons (first l) MT-LIST)]))

;;------------------------------------------------------------------------------

;; draw-editor : Editor -> Image
;; the function consumes an Editor e and returns an image containing the cursor
;; and the text before and after the cursor

;; FUNCTION EXAMPLES :

(begin-for-test 
  (check-equal?  (draw-editor (make-editor R-LO1S-HELL LO1S-O ZERO))
                 (overlay/align "left"
                                "middle"
                                (beside TEXT-HELL CURSOR TEXT-O) 
                                EDITOR-BODY)))

;; DESIGN STRATEGY: Data decomposition of e : Editor

(define (draw-editor e) 
  (local (; string that can displayed before the cursor
          (define print-pre (get-print-ls (editor-before e)
                                          (- CHAT-TXT-WIDTH
                                             (editor-plen e))))
          ; text representation of print-pre
          (define pre-text (text (implode print-pre) FONT-SIZE COLOR))
          ; width allowed for string after cursor
          (define post-width (- CHAT-TXT-WIDTH (image-width pre-text)))
          ; string that can displayed after the cursor
          (define print-post (get-print-ls (editor-after e) post-width)))
    ;--IN--
    (overlay/align
     LEFT-ALIGN
     MIDDLE-ALIGN
     (beside (text (implode (reverse print-pre)) FONT-SIZE COLOR) 
             CURSOR 
             (text (implode print-post) FONT-SIZE COLOR))
     EDITOR-BODY)))

;; Tests:

(begin-for-test 
  (check-equal?  (draw-editor (make-editor LO1S-EMPTY LO1S-HELLO ZERO))
                 (overlay/align "left"
                                "middle"
                                (beside TEXT-EMPTY CURSOR TEXT-HELLO) 
                                EDITOR-BODY)
                 "first argument is an empty string")
  (check-equal?  (draw-editor (make-editor R-LO1S-HELLO LO1S-EMPTY ZERO))
                 (overlay/align "left"
                                "middle"
                                (beside TEXT-HELLO CURSOR TEXT-EMPTY) 
                                EDITOR-BODY)
                 "second argument is an empty string")
  (check-equal?  (draw-editor (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                 (overlay/align "left"
                                "middle"
                                (beside TEXT-EMPTY CURSOR TEXT-EMPTY) 
                                EDITOR-BODY)
                 "both arguments are empty strings"))

;;------------------------------------------------------------------------------

;; edit: Editor KeyEvent -> Editor
;; Consumes an Editor e and a KeyEvent k and returns an Editor like e which 
;; represents one of the following
;; 1) Left or right movement of the cursor with the left or right keys
;; 2) Character to the left of the cursor deleted with the backspace key
;; 3) If k is a single character String other than tab or rubout, string k is 
;;    inserted where the cursor is placed
;; Also, the function limits the length of the text inside the editor to the 
;; length of the editor

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) LEFT)
                (make-editor R-LO1S-WO LO1S-RLD ZERO)))

;; DESIGN STRATEGY : data decomposition on k : KeyEvent

(define (edit e k)
  (cond
    [(key=? k LEFT)  (key-left e)]
    [(key=? k RIGHT) (key-right e)]
    [(key=? BACK-SPACE k) (key-back e)]
    [(key=? ENTER k) e]
    [(or (key=? k TAB) (key=? k RUBOUT))  e]
    [(= (string-length k) 1) (key-single e k)]
    [else e]))

;; Tests:

(begin-for-test
  (check-equal? (edit (make-editor R-LO1S-WO LO1S-RLD ZERO) RIGHT)
                (make-editor R-LO1S-WOR LO1S-LD ZERO)
                "should return editor with cursor moved right")
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) BACK-SPACE)
                (make-editor R-LO1S-WO LO1S-LD ZERO)
                "should return editor with the character to the left of the
                 cursor deleted")
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) TAB)
                (make-editor R-LO1S-WOR LO1S-LD ZERO)
                "Tab key is ignored. Editor should be returned as is")
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) ENTER)
                (make-editor R-LO1S-WOR LO1S-LD ZERO)
                "Enter key is ignored. Editor should be returned as is")
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) RUBOUT)
                (make-editor R-LO1S-WOR LO1S-LD ZERO)
                "Rubout key is ignored. Editor should be returned as is")
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) R)
                (make-editor R-LO1S-WORR LO1S-LD ZERO)
                "for a single character key, function should return an Editor 
                 with the single character inserted to the left of the cursor")
  (check-equal? (edit (make-editor R-LO1S-WOR LO1S-LD ZERO) DOWN)
                (make-editor R-LO1S-WOR LO1S-LD ZERO)
                "All other keys are ignored. 'down' key should be ignored and 
                 Editor returned as is"))

;;------------------------------------------------------------------------------

;; string->editor : String -> Editor
;; Returns an Editor containing str and cursor at position 0.

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (string->editor "hello")
                (make-editor LO1S-EMPTY LO1S-HELLO ZERO)
                "should return editor with cursor at position 0")
  (check-equal? (string->editor "")
                (make-editor LO1S-EMPTY LO1S-EMPTY ZERO)
                "Input text is empty. should return editor with cursor at 
                 position 0"))

;; DESIGN STRATEGY : Function composition

(define (string->editor str) 
  (make-editor MT-LIST (explode str) ZERO))

;;------------------------------------------------------------------------------


; editor-pos : Editor -> Natural
; Returns the position of the cursor in editor e.

(begin-for-test
  (check-equal? (editor-pos (make-editor R-LO1S-WOR LO1S-LD ZERO))
                3
                "Should return the current position of the cursor"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (editor-pos e) 
  (length (editor-before e)))

;; Tests:

(begin-for-test
  (check-equal? (editor-pos (make-editor LO1S-EMPTY LO1S-HELLO ZERO))
                0
                "Cursor is flush to the left. Should return zero")
  (check-equal? (editor-pos (make-editor R-LO1S-HELLO LO1S-EMPTY ZERO))
                5
                "Cursor is flush to the right. Should return last position")
  (check-equal? (editor-pos (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                0
                "Editor is empty. Should return zero"))

;;------------------------------------------------------------------------------

;; plen-max: NonNegReal -> NonNegReal
;; limits len to a maximum of CHAT-TXT-WIDTH

;; Examples:

(begin-for-test
  (check-equal? (plen-max ZERO)
                ZERO
                "len is less than CHAT-TXT-WIDTH")
  (check-equal? (plen-max (+ CHAT-TXT-WIDTH CHAT-TXT-WIDTH))
                CHAT-TXT-WIDTH
                "limits len to CHAT-TXT-WIDTH"))

;; Strategy: Function composition

(define (plen-max len)
  (if (> len CHAT-TXT-WIDTH)
      CHAT-TXT-WIDTH
      len))

;;------------------------------------------------------------------------------

;; plen-str-max: String NonNegReal -> NonNegReal
;; limits the len to a maximum of the width of the text for str

;; Examples:

(begin-for-test
  (check-equal? (plen-str-max SOME-STRING CHAT-TXT-WIDTH)
                (image-width (text SOME-STRING FONT-SIZE COLOR))
                "limits len to text width")
  (check-equal? (plen-str-max SOME-STRING ZERO)
                ZERO
                "len is less than text width"))

;; Strategy: Function composition

(define (plen-str-max str len)
  (if (> len (image-width (text str FONT-SIZE COLOR)))
      (image-width (text str FONT-SIZE COLOR))
      len))

;;------------------------------------------------------------------------------

;; plen-min NonNegReal -> NonNegReal
;; limits len to a minimum of ZERO

;; Examples:

(begin-for-test
  (check-equal? (plen-min CHAT-TXT-WIDTH)
                CHAT-TXT-WIDTH
                "is positive")
  (check-equal? (plen-min (- ZERO CHAT-TXT-WIDTH))
                ZERO
                "limits len to ZERO"))

;; Strategy: Function composition

(define (plen-min len)
  (if (<= len ZERO)
      ZERO
      len))

;;------------------------------------------------------------------------------

;; get-editor-post-left: Editor -> String
;; returns a String that represents (editor-after e) with the cursor moved left

;; Examples:

(begin-for-test
  (check-equal? (get-editor-post-left EDITOR-EMPTY)
                MT-STRING
                "empty editor")
  (check-equal? (get-editor-post-left EDITOR-HELL-O)
                "lo"
                "returns post string with cursor moved left"))

;; Strategy: Data decomposition on e: Editor

(define (get-editor-post-left e)
  (implode (append (mt-checked-first-list (editor-before e)) (editor-after e))))

;;------------------------------------------------------------------------------

;; chat-width>? : String -> Boolean
;; checks if the width of the text representation of str is greater than 
;; CHAT-TXT-WIDTH

;; Examples:

(begin-for-test
  (check-equal? (chat-width>? VERY-LONG-STRING)
                true
                "text of str is longer than CHAT-TXT-WIDTH")
  (check-equal? (chat-width>? SOME-STRING)
                false
                "text of str is not longer than CHAT-TXT-WIDTH"))

;; Strategy: Function composition

(define (chat-width>? str)
  (> (image-width (text str FONT-SIZE COLOR)) CHAT-TXT-WIDTH))

;;------------------------------------------------------------------------------

;; plen-left: Editor -> NonNegReal
;; calculates updated (editor-plen e) for left key press

;; Examples:

(begin-for-test
  (check-equal? (plen-left EDITOR-EMPTY)
                ZERO
                "no change in plen")
  (check-equal? (plen-left EDITOR-FULL)
                (image-width (text "@" FONT-SIZE COLOR))
                "new plen calculated"))

;; Strategy: Data decomposition on e: Editor

(define (plen-left e)
  (if (chat-width>? (get-editor-str e))
      (local (; the 1String before the cursor
              (define 1s (implode (mt-checked-first-list (editor-before e))))
              ; length of the text image of 1s
              (define txt-len (image-width (text 1s FONT-SIZE COLOR))))
      (plen-max (plen-str-max (get-editor-post-left e) 
                               (+ (editor-plen e) txt-len))))
      ZERO))

;;------------------------------------------------------------------------------

;; remove-1s-before-cursor: Editor -> Editor
;; Removes the character before the cursor in editor e

;; Examples:

(begin-for-test
  (check-equal? (remove-1s-before-cursor EDITOR-EMPTY)
                EDITOR-EMPTY
                "nothing to remove")
  (check-equal? (remove-1s-before-cursor (make-editor R-LO1S-WOR LO1S-LD ZERO))
                (make-editor R-LO1S-WO LO1S-LD ZERO)
                "character before cursor removed"))

;; Strategy: Data decomposition on e: Editor

(define (remove-1s-before-cursor e)
  (make-editor
   (mt-checked-rest-list (editor-before e))
   (editor-after e)
   (editor-plen e)))

;;------------------------------------------------------------------------------

;; add-1s-before-cursor: Editor KeyEvent -> Editor
;; adds k before the cursor in editor e

;; Examples:

(begin-for-test
  (check-equal? (add-1s-before-cursor EDITOR-EMPTY O)
                (make-editor LO1S-O LO1S-EMPTY ZERO)
                "k added before cursor")
  (check-equal? (add-1s-before-cursor (make-editor R-LO1S-WOR LO1S-LD ZERO) R)
                (make-editor R-LO1S-WORR LO1S-LD ZERO)
                "k added before cursor"))

;; Strategy: Data decomposition on e: Editor

(define (add-1s-before-cursor e k)
  (make-editor
   (cons k (editor-before e))
   (editor-after e)
   (editor-plen e)))

;;------------------------------------------------------------------------------

;; plen-back: Editor -> NonNegReal
;; calculates updated (editor-plen e) for back key press

;; Examples:

(begin-for-test
  (check-equal? (plen-back EDITOR-EMPTY)
                ZERO
                "no change in plen")
  (check-equal? (plen-back EDITOR-FULL)
                (image-width (text "@" FONT-SIZE COLOR))
                "new plen calculated"))

;; Strategy: Data decomposition on e: Editor

(define (plen-back e)
  (if (chat-width>? (get-editor-str (remove-1s-before-cursor e)))
      (local (; the 1String before the cursor
              (define 1s (implode (mt-checked-first-list (editor-before e))))
              ; length of the text image of 1s
              (define txt-len (image-width (text 1s FONT-SIZE COLOR))))
      (plen-max (plen-str-max (get-editor-post e) 
                               (+ (editor-plen e) txt-len))))
      ZERO))

;;------------------------------------------------------------------------------

;; plen-right: Editor -> NonNegReal
;; calculates updated (editor-plen e) for right key press

;; Examples:

(begin-for-test
  (check-equal? (plen-right EDITOR-EMPTY)
                ZERO
                "no change in plen")
  (check-equal? (plen-right EDITOR-FULL)
                ZERO
                "no change in plen"))

;; Strategy: Data decomposition on e: Editor

(define (plen-right e)
  (if (chat-width>? (get-editor-str e))
      (local (; the 1String after the cursor
              (define 1s (implode (mt-checked-first-list (editor-after e))))
              ; length of the text image of 1s
              (define txt-len (image-width (text 1s FONT-SIZE COLOR))))
      (plen-min (- (editor-plen e) txt-len)))
      ZERO))

;;------------------------------------------------------------------------------

;; plen-single: Editor -> NonNegReal
;; calculates updated (editor-plen e) for right key press

;; Examples:

(begin-for-test
  (check-equal? (plen-single EDITOR-EMPTY R)
                ZERO
                "no change in plen")
  (check-equal? (plen-single EDITOR-FULL R)
                ZERO
                "no change in plen"))

;; Strategy: Data decomposition on e: Editor

(define (plen-single e k)
  (if (chat-width>? (get-editor-str (add-1s-before-cursor e k)))
      (local (; length of the text image of k
              (define txt-len (image-width (text k FONT-SIZE COLOR))))
      (plen-min (- (editor-plen e) txt-len)))
      ZERO))

;;------------------------------------------------------------------------------

;; left-key : Editor -> Editor
;; given an editor e returns an editor like e with the cursor moved left

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (key-left (make-editor R-LO1S-WOR LO1S-LD ZERO))
                (make-editor R-LO1S-WO LO1S-RLD ZERO)
                "Result represents the movement of the cursor to the left"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (key-left e)
  (make-editor
   (mt-checked-rest-list (editor-before e))
   (append (mt-checked-first-list (editor-before e)) (editor-after e))
   (plen-left e)))

;; Tests:

(begin-for-test
  (check-equal? (key-left (make-editor LO1S-EMPTY LO1S-HELLO ZERO))
                (make-editor LO1S-EMPTY LO1S-HELLO ZERO)
                "Cursor is already flush to the left. Should return editor 
                 as is")
  (check-equal? (key-left (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                (make-editor LO1S-EMPTY LO1S-EMPTY ZERO)
                "The text is empty. Should return editor as is"))

;;------------------------------------------------------------------------------

;; key-right : Editor -> Editor
;; given an editor e returns an editor like e with the cursor moved right

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (key-right (make-editor R-LO1S-WO LO1S-RLD ZERO))
                (make-editor R-LO1S-WOR LO1S-LD ZERO)
                "Result represents the movement of the cursor to the right"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (key-right e)
  (make-editor
   (append (mt-checked-first-list (editor-after e)) (editor-before e))
   (mt-checked-rest-list (editor-after e))
   (plen-right e)))

;; Tests:

(begin-for-test
  (check-equal? (key-right (make-editor R-LO1S-HELLO LO1S-EMPTY ZERO))
                (make-editor R-LO1S-HELLO LO1S-EMPTY ZERO)
                "Cursor is already flush to the right. Should return editor 
                 as is")
  (check-equal? (key-right (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                (make-editor LO1S-EMPTY LO1S-EMPTY ZERO)
                "The text is empty. Should return editor as is"))

;;------------------------------------------------------------------------------

;; key-back : Editor -> Editor
;; given an editor e returns an editor like e with the character to the left of
;; the cursor deleted

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (key-back (make-editor R-LO1S-WOR LO1S-LD ZERO))
                (make-editor R-LO1S-WO LO1S-LD ZERO)
                "Should delete character to the left of the cursor"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (key-back e)
  (make-editor
   (mt-checked-rest-list (editor-before e))
   (editor-after e)
   (plen-back e)))

;; Tests:

(begin-for-test
  (check-equal? (key-back (make-editor LO1S-EMPTY LO1S-HELLO ZERO))
                (make-editor LO1S-EMPTY LO1S-HELLO ZERO)
                "Cursor is already flush to the left. Should return editor 
                 as is")
  (check-equal? (key-back (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                (make-editor LO1S-EMPTY LO1S-EMPTY ZERO)
                "The text is empty. Should return editor as is"))

;;------------------------------------------------------------------------------

;; key-single : Editor KeyEvent -> Editor
;; given an editor e returns an editor like e with k entered to the left of  
;; the cursor

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (key-single (make-editor R-LO1S-WOR LO1S-LD ZERO) R)
                (make-editor R-LO1S-WORR LO1S-LD ZERO)
                "Should enter character to the left of the cursor"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (key-single e k)
  (make-editor
   (cons k (editor-before e))
   (editor-after e)
   (plen-single e k)))

;; Tests:

(begin-for-test
  (check-equal? (key-single (make-editor LO1S-EMPTY LO1S-WOR ZERO) O)
                (make-editor LO1S-O LO1S-WOR ZERO)
                "Cursor is flush to the left. Should enter 'v' as the first
                 character")
  (check-equal? (key-single (make-editor LO1S-EMPTY LO1S-EMPTY ZERO) O)
                (make-editor LO1S-O LO1S-EMPTY ZERO)
                "The editor is empty. Should return editor with 'v' as the 
                 only character"))

;;------------------------------------------------------------------------------

;; get-editor-pre : Editor -> String
;; Returns the text in editor e before the cursor.

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (get-editor-pre (make-editor R-LO1S-WOR LO1S-LD ZERO))
                "wor"
                "Should return the string before the cursor")
  (check-equal? (get-editor-pre (make-editor LO1S-EMPTY LO1S-HELLO ZERO))
                MT-STRING
                "Cursor is flush to the left. Empty string should be 
                 returned"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (get-editor-pre e) 
  (implode (reverse (editor-before e))))

;; TESTS :

(begin-for-test
  (check-equal? (get-editor-pre (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                MT-STRING
                "Editor is empty. Empty string should be 
                 returned"))

;;------------------------------------------------------------------------------

; get-editor-post : Editor -> String
; Returns the text in editor e after the cursor.

;; FUNCTION EXAMPLES :

(begin-for-test
  (check-equal? (get-editor-post (make-editor  R-LO1S-WOR LO1S-LD ZERO))
                "ld"
                "Should return the string before the cursor")
  (check-equal? (get-editor-post (make-editor R-LO1S-HELLO LO1S-EMPTY ZERO))
                MT-STRING
                "Cursor is flush to the right. Empty string should be 
                 returned"))

;; DESIGN STRATEGY : Data decomposition on e : Editor

(define (get-editor-post e) 
  (implode (editor-after e)))

;; TESTS :

(begin-for-test
  (check-equal? (get-editor-post (make-editor LO1S-EMPTY LO1S-EMPTY ZERO))
                MT-STRING
                "Editor is empty. Empty string should be 
                 returned"))

;;------------------------------------------------------------------------------

;; get-print-ls: ListOf<1String> NonNegReal -> ListOf<1String>
;; returns the maximum number of 1String that can be printed in given width

;; Example:

(begin-for-test
  (check-equal? (get-print-ls (list "a" "b" "c" "d") 10)
                (list "a")
                "only a can be fit into given width")
  (check-equal? (get-print-ls (list "a" "b" "c" "d") 100)
                (list "a" "b" "c" "d")
                "entire list can be fit into given width"))

;; Strategy: Function composition

(define (get-print-ls ls0 width)
  (local (; get-print-ls/a: ListOf<1String> ListOf<1String> Boolean 
          ;                 -> ListOf<1String>
          ; returns the maximum number of 1String that can be printed in width
          ; Where: a is the ListOf<1String> from ls that can be fit in width
          ; so far and continue? checks if final the result is obtained
          ; Strategy: Data decomposition on ls: ListOf<1String>
          (define (get-print-ls/a ls a continue?)
            (cond
              [(empty? ls) a]
              [else (local (; possible string that could fit i width
                            (define pos-a (append a (list (first ls))))
                            ; text representation of pos-a
                            (define txt (text (implode pos-a) FONT-SIZE COLOR))
                            ; length of txt
                            (define text-len (image-width txt))
                            ; updating accumulator continue?
                            (define new-continue? (and (<= text-len width)
                                                       continue?))
                            ; updating accumulator a
                            (define new-a (if new-continue?
                                              pos-a
                                              a)))
                      (get-print-ls/a (rest ls) new-a new-continue?))])))
    ;--IN--
    (get-print-ls/a ls0 MT-LIST true)))

;;------------------------------------------------------------------------------

;; string-split-length : String NonNegReal -> ListOf<String>
;; Returns a list of strings by splitting str0, such that each element of the 
;; list fits in width

;; Examples:

(begin-for-test
  (check-equal? (string-split-length "hello"
                                     (image-width (text "hello" 
                                                        FONT-SIZE 
                                                        COLOR)))
                (list "hello")
                "hello fits in given width")
  (check-equal? (string-split-length "hello"
                                     (image-width (text "hell" 
                                                        FONT-SIZE 
                                                        COLOR)))
                (list "hell" "o")
                "split into two lines"))

;; Strategy: Function composition

(define (string-split-length str0 width)
  (local (; string-split-length/a : String ListOf<String> -> ListOf<String>
          ; Returns a list of strings by splitting str, such that each element 
          ; of the list fits in width
          ; Where: a is the list of strings which are part of str which can be
          ; fit in given width so far
          ; Strategy: Function composition
          (define (string-split-length/a str a)
            (local (; part of str that fits in width
                    (define word-fit (str-for-width str width))
                    ; part of str that is beyond width
                    (define word-extra (str-after-width str width)))
              (if (string=? word-extra MT-STRING)
                (append a (list word-fit))
                (string-split-length/a word-extra 
                                       (append a (list word-fit)))))))
    ;--IN--
    (string-split-length/a str0 MT-LIST)))

;;------------------------------------------------------------------------------
     
;; string-split-to-fit : String NonNegReal -> ListOf<String>
;; splits string into a list of words. If a word doesn't fit in given width, it
;; is split into smaller words and is added to the output list

;; Examples:

(begin-for-test
  (check-equal? (string-split-to-fit "hello world"
                                     (image-width (text "hello" 
                                                        FONT-SIZE 
                                                        COLOR)))
                (list "hello" "worl" "d")
                "world is split into 2")
  (check-equal? (string-split-to-fit "hello world"
                                     (image-width (text "world" 
                                                        FONT-SIZE 
                                                        COLOR)))
                (list "hello" "world")
                "split into two lines"))

;; Strategy: Function composition

(define (string-split-to-fit str width)
  (local (; fit-word : String ListOf<String> -> ListOf<String>
          ; adds w to b if it fits in width. if w doesn't it in width, it is 
          ; split into smaller parts and is added to b
          ; Strategy: Function composition
          (define (fit-word w b)
            (if (> (image-width (text w FONT-SIZE COLOR)) width)
                (append (string-split-length w width) b)
                (append (list w) b))))
    ;--IN--
    (foldr fit-word MT-LIST (string-split str))))

;;------------------------------------------------------------------------------

;; words-after-width: String NonNegReal -> String
;; returns the words of String str that can not be printed in given width

;; Example:

(begin-for-test
  (check-equal? (words-after-width "hello world" 
                                 (image-width (text "world" FONT-SIZE COLOR)))
                "world"
                "world can't be fit in given width")
  (check-equal? (words-after-width "hello world" 
                               (image-width (text "hello world" 
                                                  FONT-SIZE 
                                                  COLOR)))
                MT-STRING
                "entire string can be fit into given width"))

;; Strategy: Function composition

(define (words-after-width str width)
  (local (; words-for-width/a: ListOf<String> ListOf<String> ListOf<String>
          ;                    Boolean -> ListOf<String>
          ; returns the ListOf<String> that can not be printed in width
          ; Where: in is the ListOf<String> from ls that can be fit in width
          ; so far, a is the ListOf<String> from ls that can not be fit in 
          ; width so far and full? checks if final 'in' is obtained
          ; Strategy: Data decomposition on ls: ListOf<String>
          (define (wrds-after-width/a ls in a full?)
            (cond
              [(empty? ls) (string-join a)]
              [else (local (; updating accumulator 'in'
                            (define new-in (append in (list (first ls))))
                            ; text representation of new-in
                            (define txt (text (string-join new-in) 
                                              FONT-SIZE 
                                              COLOR))
                            ; length of txt
                            (define text-len (image-width txt))
                            ; updating accumulator full?
                            (define new-full? (or (> text-len width)
                                                  full?))
                            ; updating accumulator a
                            (define new-a (if new-full?
                                              (append a (list (first ls)))
                                              a)))
                      (wrds-after-width/a (rest ls) new-in new-a new-full?))])))
    ;--IN--
    (if (<= (image-width (text str FONT-SIZE COLOR)) width)
        MT-STRING
        (wrds-after-width/a (string-split-to-fit str width) 
                            MT-LIST MT-LIST false))))

;;------------------------------------------------------------------------------

;; words-for-width: String NonNegReal -> String
;; returns the words of String str that can be printed in given width

;; Example:

(begin-for-test
  (check-equal? (words-for-width "hello world" 
                                 (image-width (text "hello" FONT-SIZE COLOR)))
                "hello"
                "only hello can be fit into given width")
  (check-equal? (words-for-width "hello world" 
                               (image-width (text "hello world" 
                                                  FONT-SIZE 
                                                  COLOR)))
                "hello world"
                "entire string can be fit into given width"))

;; Strategy: Function composition

(define (words-for-width str width)
  (local (; get-print-ls/a: ListOf<1String> ListOf<1String> Boolean 
          ;                 -> ListOf<1String>
          ; returns the maximum number of words that can be printed in width
          ; Where: a is the ListOf<String> from ls that can be fit in width
          ; so far and continue? checks if final the result is obtained
          ; Strategy: Data decomposition on ls: ListOf<String>
          (define (words-for-width/a ls a continue?)
            (cond
              [(empty? ls) (string-join a)]
              [else (local (; possible words that could fit in width
                            (define pos-a (append a (list (first ls))))
                            ; text representation of pos-a
                            (define txt (text (string-join pos-a) 
                                              FONT-SIZE 
                                              COLOR))
                            ; length of txt
                            (define text-len (image-width txt))
                            ; updating accumulator continue?
                            (define new-continue? (and (<= text-len width)
                                                       continue?))
                            ; updating accumulator a
                            (define new-a (if new-continue?
                                              pos-a
                                              a)))
                      (words-for-width/a (rest ls) new-a new-continue?))])))
    ;--IN--
    (words-for-width/a (string-split-to-fit str width) MT-LIST true)))

;;------------------------------------------------------------------------------

;; str-for-width: String NonNegReal -> String
;; returns the part of String str that can be printed in given width

;; Example:

(begin-for-test
  (check-equal? (str-for-width "abcdefg" 
                               (image-width (text "a" FONT-SIZE COLOR)))
                "a"
                "only a can be fit into given width")
  (check-equal? (str-for-width "abcdefg" 
                               (image-width (text "abcdefg" FONT-SIZE COLOR)))
                "abcdefg"
                "entire string can be fit into given width"))

;; Strategy: Function composition

(define (str-for-width str width)
  (if (<= (image-width (text str FONT-SIZE COLOR)) width)
      str
      (implode (get-print-ls (explode str) width))))

;;------------------------------------------------------------------------------

;; str-after-width String NonNegReal -> String
;; returns the part of String str that can not be printed in given width

;; Example:

(begin-for-test
  (check-equal? (str-after-width "abcdefg" 
                                 (image-width (text "a" FONT-SIZE COLOR)))
                "bcdefg"
                "'bcdefg' can not be fit into given width")
  (check-equal? (str-after-width "abcdefg" 
                                 (image-width (text "abcdefg" FONT-SIZE COLOR)))
                MT-STRING
                "entire string can be fit into given width"))

;; Strategy: Function composition

(define (str-after-width str width)
  (local (; str-after-width/a: ListOf<1String> ListOf<1String> ListOf<1String>
          ;                    Boolean -> ListOf<1String>
          ; returns the ListOf<1String> that can not be printed in width
          ; Where: in is the ListOf<1String> from ls that can be fit in width
          ; so far, a is the ListOf<1String> from ls that can not be fit in 
          ; width so far and full? checks if final 'in' is obtained
          ; Strategy: Data decomposition on ls: ListOf<1String>
          (define (str-after-width/a ls in a full?)
            (cond
              [(empty? ls) (implode a)]
              [else (local (; updating accumulator 'in'
                            (define new-in (append in (list (first ls))))
                            ; text representation of new-in
                            (define txt (text (implode new-in) FONT-SIZE COLOR))
                            ; length of txt
                            (define text-len (image-width txt))
                            ; updating accumulator full?
                            (define new-full? (or (> text-len width)
                                                  full?))
                            ; updating accumulator a
                            (define new-a (if new-full?
                                              (append a (list (first ls)))
                                              a)))
                      (str-after-width/a (rest ls) new-in new-a new-full?))])))
    ;--IN--
    (if (<= (image-width (text str FONT-SIZE COLOR)) width)
        MT-STRING
        (str-after-width/a (explode str) MT-LIST MT-LIST false))))

;;------------------------------------------------------------------------------


;; get-color: MsgForClient -> Color
;; returns the text color of the given MsgForClient msg

;; Examples:

(begin-for-test
  (check-equal? (get-color CLIENT-JOIN)
                JL-CLR
                "returns text color")
  (check-equal? (get-color CLIENT-LEAVE)
                JL-CLR
                "returns text color")
  (check-equal? (get-color CLIENT-ERROR)
                ERROR-CLR
                "returns text color")
  (check-equal? (get-color CLIENT-PRIVATE)
                PVT-CLR
                "returns text color")
  (check-equal? (get-color CLIENT-BROADCAST)
                COLOR
                "returns text color"))

;; Strategy: Data decomposition on msg: MsgForClient

(define (get-color msg)
  (cond
    [(join-mc? msg) JL-CLR]
    [(leave-mc? msg) JL-CLR]
    [(error-mc? msg) ERROR-CLR]
    [(private-mc? msg) PVT-CLR]
    [(broadcast-mc? msg) COLOR]))

;;------------------------------------------------------------------------------

;; print-msgs: ListOf<MsgForClient> Image -> Image
;; print ListOf<MsgForClient> ls0 in img

;; Examples:

(begin-for-test
  (check-equal? (print-msgs MT-LIST NAMES-AREA)
                NAMES-AREA
                "nothing to print")
  (check-equal? (print-msgs CHAT-LIST-LONG NAMES-AREA)
                (place-image HALF-LONG-TEXT
                             (/ (image-width HALF-LONG-TEXT) TWO)
                             START-Y
                             (place-image HALF-LONG-TEXT
                                          (/ (image-width HALF-LONG-TEXT) TWO)
                                          (+ START-Y TEXT-HEIGHT)
                                          NAMES-AREA))
                "string split into two lines"))


;; Strategy: Function composition

(define (print-msgs ls0 img)
  (local (; width of the given image
          (define width (image-width img))
          ; print-msgs/a: ListOf<MsgForClient> Coordinate Image -> Image
          ; renders an image of the ListOf<MsgForClient> ls on the image img
          ; Where: y is the y coordinate where the current message should be 
          ; printed and i is the image of the messages printed so far
          (define (print-msgs/a ls y i)
            (cond
              [(empty? ls) i]
              [else (local (; updated accumulator y
                            (define new-y (+ y TEXT-HEIGHT))
                            ; the type of MsgForClient
                            (define type (first (first ls)))
                            ; content of the message
                            (define msg (second (first ls)))
                            ; part of the message that can be printed in 1 line
                            (define pos-str (words-for-width msg width))
                            ; part of the message that goes beyong 1 line
                            (define after-str (words-after-width msg width))
                            ; message to be printed in next line
                            (define next-msg (list type after-str))
                            ; color of the text to be printed
                            (define color (get-color (first ls)))
                            ; text representation of the message
                            (define txt (text pos-str FONT-SIZE color))
                            ; x coordinate of the current line
                            (define x (/ (image-width txt) TWO))
                            ; new list for the the next recursion
                            (define new-ls (if (string=? after-str MT-STRING)
                                               (rest ls)
                                               (cons next-msg (rest ls))))
                            ; image with current message printed
                            (define new-i (place-image txt x y i)))
                      (print-msgs/a new-ls new-y new-i))]))) 
    ;--IN--
    (print-msgs/a ls0 START-Y img)))

;;------------------------------------------------------------------------------

;; string-ci-sf<?: UserName UserName -> Boolean
;; lexicographically compares str1 and str2

;; Examples:

(begin-for-test
  (check-equal? (string-ci-sf<? "abc" "xyz")
                true
                "'abc' is lexicographically before 'xyz'")
  (check-equal? (string-ci-sf<? "xyz" "abc")
                false
                "'xyz' is not lexicographically before 'abc'"))

;; Strategy: Function composition

(define (string-ci-sf<? str1 str2)
  (if (string-ci=? str1 str2)
      (string>? str1 str2)
      (string-ci<? str1 str2)))

;;------------------------------------------------------------------------------

;; extension-of-nam? : String -> Boolean
;; Returns true if pre is a prefix of str.

;; Examples:

(begin-for-test
  (check-equal? (extension-of-nam? "abc" "abcd")
                true
                "'abc' is an extension of 'abcd'")
  (check-equal? (extension-of-nam? "xyz" "abc")
                false
                "'xyz' is not an extension of 'abc'"))

;; Strategy: function composition

(define (extension-of-nam? pre str)
  (and (<= (string-length pre) (string-length str))
       (string=? pre (substring str ZERO (string-length pre)))))

;;------------------------------------------------------------------------------

;; find-name : String ListOf<Username> -> Boolean
;; returns the first Username in names which has a prefix nam. If there is no 
;; match nam is returned

(begin-for-test
  (check-equal? (find-name "chat" USERLIST)
                USERNAME1
                "match found")
  (check-equal? (find-name "xyz" USERLIST)
                "xyz"
                "no match found"))

;; Strategy: function composition

(define (find-name nam names)
  (local (; get-extended-name: String String -> String
          ; returns n if n is an extension of b
          ; Strategy: Function composition
          (define (get-extended-name n b)
            (if (and (extension-of-nam? nam n) (string=? b nam))
                n
                b)))
    ;--IN--
    (foldl get-extended-name nam names)))

;;------------------------------------------------------------------------------

;; format-from-msg : MsgFromServer -> String
;; returns the string representation of the MsgFromServer msg
;; Where: msg is not of the type (list 'userlist ListOf<UserName>)

;; Examples:

(begin-for-test
  (check-equal? (format-from-msg FROM-SERVER-JOIN)
                (string-append USERNAME1 " joined.")
                "user joined"))

;; Strategy: Data decomposition on msg: MsgFromServer

(define (format-from-msg msg)
  (cond
    [(join? msg) (string-append (second msg) JOIN-POST-STR)]
    [(leave? msg) (string-append (second msg) LEAVE-POST-STR)]
    [(error? msg) (second msg)]
    [(private-from? msg) (string-append OPEN-BRACKET (second msg) CLOSE-BRACKET
                                        (third msg))]
    [(broadcast-from? msg) (string-append OPEN-BRACKET (second msg) 
                                          CLOSE-BRACKET (third msg))]))

;; Tests:

(begin-for-test
  (check-equal? (format-from-msg FROM-SERVER-LEAVE)
                (string-append USERNAME1 " left the chat.")
                "user left")
  (check-equal? (format-from-msg FROM-SERVER-ERROR)
                "user does not exist"
                "error message")
  (check-equal? (format-from-msg FROM-SERVER-PRIVATE)
                (string-append "< " USERNAME1 " > " MESSAGE)
                "private message")
  (check-equal? (format-from-msg FROM-SERVER-BROADCAST)
                (string-append "< " USERNAME2 " > " MESSAGE)
                "broadcast message"))

;;------------------------------------------------------------------------------

;; format-to-msg : Username MsgToServer -> String
;; returns the string representation of the MsgToServer msg

;; Examples:

(begin-for-test
  (check-equal? (format-to-msg USERNAME2 TO-SERVER-PRIVATE)
                (string-append "< " USERNAME2 "->" USERNAME1 " > " MESSAGE)
                "private message")
  (check-equal? (format-to-msg USERNAME1 TO-SERVER-BROADCAST)
                (string-append "< " USERNAME1 " > " MESSAGE)))

;; Strategy: Data decomposition on msg: MsgToServer

(define (format-to-msg name msg)
  (cond
    [(private-to? msg) (string-append OPEN-BRACKET name ARROW (second msg) 
                                      CLOSE-BRACKET (third msg))]
    [(broadcast-to? msg) (string-append OPEN-BRACKET name CLOSE-BRACKET 
                                        (second msg))]))

;;------------------------------------------------------------------------------

;; lines-for-width: String NonNegReal -> Natural
;; returns the number of lines it takes to print str0 in the given width

(begin-for-test
  (check-equal? (lines-for-width LONG-STRING NAMES-AREA-WIDTH)
                2
                "long string")
  (check-equal? (lines-for-width SOME-STRING NAMES-AREA-WIDTH)
                1
                "short string"))

;; Strategy: Function composition

(define (lines-for-width str0 width)
  (local (; lines-for-width/a: String Natural
          ; returns the number of lines it takes to print str in the given width
          ; Where a is the number of lines it takes to print the part of str0 
          ; excluding str
          ; Strategy: Function composition
          (define (lines-for-width/a str a)
            (if (string=? (words-after-width str width) MT-STRING)
                a
                (lines-for-width/a (words-after-width str width) (add1 a)))))
    ;--IN--
    (lines-for-width/a str0 ONE)))

;;------------------------------------------------------------------------------

;; msg-list-that-fits: ListOf<MsgForClient> NonNegReal NonNegReal 
;;                     -> ListOf<MsgForClient>
;; returns the ListOf<MsgForClient> that can be fit into the given width and 
;; height in the reverse order

;; Examples: 

(begin-for-test
  (check-equal? (msg-list-that-fits CHAT-LIST NAMES-AREA-WIDTH TEXT-HEIGHT)
                (list (first CHAT-LIST))
                "only one message can be fit")
  (check-equal? (msg-list-that-fits CHAT-LIST NAMES-AREA-WIDTH CANVAS-HEIGHT)
                (reverse CHAT-LIST)
                "entire list can be fit"))

;; Strategy: Function composition

(define (msg-list-that-fits ls0 width height)
  (local (; msg-list-that-fits/a: ListOf<MsgForClient> NonNegReal
          ;                       ListOf<MsgForClient> -> ListOf<MsgForClient>
          ; returns the ListOf<MsgForClient> that can be fit into the given 
          ; width and height in the reverse order
          ; Where: l is the messages that can be fit in the given height and
          ; width so far and a is the height of the messages in l
          ; Strategy: Data decomposition on ls: ListOf<MsgForClient>
          (define (msg-list-that-fits/a ls a l)
            (cond
              [(empty? ls) l]
              [else (local ((define n (lines-for-width (second (first ls)) 
                                                       width))
                            (define msg-height (* TEXT-HEIGHT n))
                            (define new-a (+ msg-height a))
                            (define new-l (if (<= new-a height)
                                              (cons (first ls) l)
                                              l)))
                      (msg-list-that-fits/a (rest ls) new-a new-l))])))
    ;--IN--
    (msg-list-that-fits/a ls0 ZERO MT-LIST)))

;;------------------------------------------------------------------------------

;; add-user : Username ListOf<Username> -> ListOf<Username>
;; adds name to ls and sorts the new list in lexicographical order

;; Examples:

(begin-for-test
  (check-equal? (add-user USERNAME3 USERLIST)
                (cons USERNAME3 SORTED-USERLIST)
                "adds name to ls"))

;; Strategy: Function composition

(define (add-user name ls)
  (sort (cons name ls) string-ci-sf<?))

;;------------------------------------------------------------------------------

;; print-user-names:  ListOf<Username> Image -> Image
;; Prints the ls0 in img

;; Examples:

(begin-for-test
  (check-equal? (print-user-names USERLIST NAMES-AREA)
                (place-image USERNAME1-TEXT 
                             (/ (image-width USERNAME1-TEXT) TWO)
                             START-Y
                             (place-image USERNAME2-TEXT 
                                          (/ (image-width USERNAME2-TEXT) TWO)
                                          (+ START-Y TEXT-HEIGHT)
                                          NAMES-AREA))
                "usernames are printed in the given image")
  (check-equal? (print-user-names USERLIST TEST-NAMES-AREA)
                (place-image USERNAME1-TEXT 
                             (/ (image-width USERNAME1-TEXT) TWO)
                             START-Y
                             TEST-NAMES-AREA)
                "only one username can be fit"))

;; Strategy: Function composition

(define (print-user-names ls0 img)
  (local (; height of the given image
          (define img-height (image-height img))
          ; print-user-names/a: ListOf<Username> Coordinate Image -> Image
          ; Prints the ls in img
          ; Where y is the y coordinate of username currently being printed and 
          ; i is the image with Usernames in ls0 which are not in ls
          ; Strategy: Data decomposition on ls: ListOf<Username>
          (define (print-user-names/a ls y i)
            (cond
              [(empty? ls) i]
              [else (local (; updated accumulator y
                            (define new-y (+ y TEXT-HEIGHT)) 
                            ; height of the Usernames printed so far
                            (define height (+ y (/ TEXT-HEIGHT TWO)))
                            ; text of the current Username
                            (define txt (text (first ls) FONT-SIZE COLOR))
                            ; x coordinate of the current Username
                            (define x (/ (image-width txt) TWO))
                            ; updated accumulator i
                            (define new-i (if (<= height img-height)
                                              (place-image txt x y i)
                                              i)))
                      (print-user-names/a (rest ls) new-y new-i))])))
    ;--IN--
    (print-user-names/a ls0 START-Y img)))

;;------------------------------------------------------------------------------

;; remove-last : NEListOf<X> -> ListOf<X>
;; removes last item from the non empty list ls

;; Examples:

(begin-for-test
  (check-equal? (remove-last (list 1 2 3 4))
                (list 1 2 3)
                "last item removed"))

;; Strategy: Data decomposition on ls: NEListOf<X>

(define (remove-last ls)
  (reverse (rest (reverse ls))))

;; Tests:

(begin-for-test
  (check-equal? (remove-last (list "string"))
                MT-LIST
                "last item removed"))

;;------------------------------------------------------------------------------

;; add-to-chat: MsgFromServer ListOf<MsgForClient> -> ListOf<MsgForClient>
;; converts msg to MsgForClient and adds it to ListOf<MsgForClient>
;; Where: msg is not of the type (list 'userlist ListOf<UserName>)

;; Examples:

(begin-for-test
  (check-equal? (add-to-chat FROM-SERVER-JOIN CHAT-LIST)
                (cons (list (first FROM-SERVER-JOIN)
                            (format-from-msg FROM-SERVER-JOIN)) CHAT-LIST)
                "message added to chat")
  (check-equal? (add-to-chat FROM-SERVER-BROADCAST CHAT-LIST-FULL)
                (cons (list (first FROM-SERVER-BROADCAST)
                            (format-from-msg FROM-SERVER-BROADCAST)) 
                      (reverse (rest (reverse CHAT-LIST-FULL))))
                "message added to chat"))

;; Strategy: Data decomposition on msg: MsgFromServer

(define (add-to-chat msg chat)
  (local (; Formatted message content
          (define str (format-from-msg msg))
          ; MsgForClient representation of msg
          (define cur-msg (list (first msg) str)))
    (if (>= (length chat) MAX-CHAT-HISTORY)
        (cons cur-msg (remove-last chat))
        (cons cur-msg chat))))

;;------------------------------------------------------------------------------

;; add-my-msg-to-chat: Username MsgToServer ListOf<MsgForClient> -> 
;;                     ListOf<MsgForClient>
;; converts msg to MsgForClient and adds it to ListOf<MsgForClient>

;; Examples:

(begin-for-test
  (check-equal? (add-my-msg-to-chat USERNAME1 TO-SERVER-PRIVATE CHAT-LIST)
                (cons (list (first TO-SERVER-PRIVATE)
                            (format-to-msg USERNAME1 TO-SERVER-PRIVATE)) 
                      CHAT-LIST)
                "message added to chat")
  (check-equal? (add-my-msg-to-chat USERNAME1 TO-SERVER-PRIVATE CHAT-LIST-FULL)
                (cons (list (first TO-SERVER-PRIVATE)
                            (format-to-msg USERNAME1 TO-SERVER-PRIVATE)) 
                      (reverse (rest (reverse CHAT-LIST-FULL))))
                "message added to chat"))

;; Strategy: Data decomposition on msg: MsgToServer

(define (add-my-msg-to-chat name msg chat)
  (local (; Formatted message content
          (define str (format-to-msg name msg))
          ; MsgForClient representation of msg
          (define cur-msg (list (first msg) str)))
    (if (>= (length chat) MAX-CHAT-HISTORY)
        (cons cur-msg (remove-last chat))
        (cons cur-msg chat))))

;;------------------------------------------------------------------------------

;; index-of: 1String str : Natural
;; returns the index of 1s in str
;; Where: 1s is present in str

;; Examples:

(begin-for-test
  (check-equal? (index-of "b" "abc")
                1
                "index of b is 1"))

;; Strategy: Function composition

(define (index-of 1s str)
  (local (; index-of/a: ListOf<1String> Natural Natural -> Natural
          ; returns the position of 1s in ls
          ; Where: i is the current position in ls and a is the position where 
          ; 1s was found
          ; Strategy: Data decomposition on ls: ListOf<1String>
          (define (index-of/a ls i a)
            (cond 
              [(empty? ls) a]
              [else (local ((define new-i (add1 i))
                            (define new-a (if (and (string=? 1s (first ls))
                                                   (= a ZERO))
                                              i
                                              a)))
                      (index-of/a (rest ls) new-i new-a))])))
    ;--IN--
    (index-of/a (explode str) ZERO ZERO)))

;;------------------------------------------------------------------------------