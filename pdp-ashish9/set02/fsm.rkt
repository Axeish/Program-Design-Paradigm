;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(define TIME-ON-TASK 16) 

(provide INITIAL-WORLD)
(provide next-state)
(provide render)
(provide stop?)
(provide accept-state?)
(provide error-state?)

;image constant
(define WHITE-FLAG (rectangle 100 100 "solid" "white")) ;Image
(define GREEN-FLAG (rectangle 100 100 "solid" "green")) ;Image
(define YELLO-FLAG (rectangle 100 100 "solid" "yellow"));Image
(define RED-FLAG (rectangle 100 100 "solid" "red"));Image

; A World is one of the:
; - AA 
; - BC 
; - DD 
; - ER 
;INTERP:tells the state of the World. state DD is accepting state
;ER is error state.AA is the initial state which will accept keys
;according to the folowing expression
;    a (b|c)* d
;is sequence of key matches then it will go to BC state else to ER state.
;when successful it will go to DD state


(define AA "start, expect to see an 'a' next")
(define BC "expect to see: 'b', 'c', or 'd'")
(define DD "encountered a 'd', finished")
(define ER "error, user pressed illegal key")

;TEMPLATE:
;world-fn: World ->???
;(define (world-fn w)
;   (cond
;     [(string=? AA w) ...]
;     [(string=? BC w) ...]
;     [(string=? DD w) ...]
;     [(string=? ER w) ...]))



;initial-world is a first state of FSM stimulation
(define INITIAL-WORLD AA)

;run: World -> nextStateWorld
;Will take initial world and start stimulation
;STRATEGY : function Composition              
(define (run w)
  
  (big-bang w
            (on-key next-state)
            (to-draw render)
            (stop-when stop?)
          ))


;next-state: worldState-> nextWorldState
;Will determine next state of the World
;Example:
(begin-for-test 
  (check-equal? (next-state AA "a") BC
                "yellow flag"))
;STRATEGY: 
;Function Composition
(define (next-state state k)
  (if (string=? state AA)
      (cond 
        [(key=? k "a") BC]
        [else ER])
      (cond
        [(key=? k "a") ER]
        [(key=? k "b") BC]
        [(key=? k "c") BC]
        [(key=? k "d") DD]
        [else ER])))

(begin-for-test 
  (check-equal? (next-state AA "a") BC
                "yellow flag")
  (check-equal? (next-state AA "b") ER
                "red flag")
  (check-equal? (next-state AA "c") ER
                "red flag")
  (check-equal? (next-state AA "d") ER
                "red flag")
  (check-equal? (next-state AA "x") ER
                "red flag")
  (check-equal? (next-state BC "a") ER
                "yellow flag")
  (check-equal? (next-state BC "b") BC
                "yellow flag")
  (check-equal? (next-state BC "c") BC
                "yellow flag")
  (check-equal? (next-state BC "d") DD
                "green flag"))
  (check-equal? (next-state BC "x") ER
                "red flag")

; render : WorldState -> Image
; Will draw flag corresponding to each state of the world.
(begin-for-test 
  (check-equal? (render AA ) WHITE-FLAG
                "white flag"))
;STRATEGY: Function Composition

(define (render cw)
    (cond
    [(eq? cw AA )WHITE-FLAG]
    [(eq? cw BC) YELLO-FLAG]
    [(eq? cw DD) GREEN-FLAG]
    [(eq? cw ER) RED-FLAG ]
    ))
(begin-for-test 
  (check-equal? (render AA ) WHITE-FLAG
                "white flag")
  (check-equal? (render BC ) YELLO-FLAG
                "yellow flag")
  (check-equal? (render DD ) GREEN-FLAG
                "green flag")
  (check-equal? (render ER ) RED-FLAG
                "red flag"))

;stop? : WorldState-> Boolean
;will stop the stimulationa nd return the current state
;HERE: the state is either accepting or error state
 (begin-for-test 
  (check-equal? (stop? DD ) #true) "stimulation stopped")
;STRATEGY: function Composition
 
(define (stop? cw) 
  (or (accept-state? cw)
      (error-state? cw)))
(begin-for-test 
  (check-equal? (stop? DD ) #true "stimulation stopped")
  (check-equal? (stop? ER ) #true "stimulation stopped")
  (check-equal? (stop? AA ) #false "stimulation continues")
  (check-equal? (stop? BC ) #false "stimulation continues"))


;accept-state?: worldState -> Boolean
;Will decide if the state has reached accpeting state 
(begin-for-test
  (check-equal? (accept-state? DD) #true "state accepted"))

;STRATEGY: Function Composition
(define (accept-state? state) 
  (string=? state DD))

(begin-for-test
  (check-equal? (accept-state? DD) #true "state accepted")
  (check-equal? (accept-state? AA) #false "state accepted")
  (check-equal? (accept-state? BC) #false "state accepted")
  (check-equal? (accept-state? ER) #false "state accepted"))


;error-state? worldState -> Boolean
;Will decide if the state has reached error state 
(begin-for-test
  (check-equal? (error-state? ER) #true "state accepted"))

;STRATEGY: Function Composition
(define (error-state? state) 
  (string=? state ER))
(begin-for-test
  (check-equal? (error-state? DD) #false "state accepted")
  (check-equal? (error-state? AA) #false "state accepted")
  (check-equal? (error-state? BC) #false "state accepted")
  (check-equal? (error-state? ER) #true "state accepted"))
(run INITIAL-WORLD)