;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-unavail) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 10 : sched-with-unavail.rkt

;; Required Libraries
;;------------------------------------------------------------------------

(require "extras.rkt")
(require rackunit)
(require "sched-general.rkt")

;; Provide Functions
;;------------------------------------------------------------------------

(provide schedule/unavail-ok?)
(provide schedule/unavail)

;; Data Definitions
;;------------------------------------------------------------------------

;;; All Data Definition defined in sched-general.rkt

;;------------------------------------------------------------------------

;; Functions

;;------------------------------------------------------------------------

;; schedule/unavail : StudentUnavails CodeWalks -> Maybe<CodeWalks>
;; Creates a CodeWalk schedule by assigning students to cws, 
;; while satisfying students' constraints.
;; Returns #f if no schedule is possible.

;; Example:

(begin-for-test
  (check-equal?
   (schedule/unavail (list ST1 ST2 ST3) CWS0)
   CWS-TEST
   "Trivial Allocation"))

; Strategy: Function Composition

(define (schedule/unavail students0 cws)
  (schedule/general students0 cws false))

;;------------------------------------------------------------------------

;; schedule/unavail-ok? : StudentUnavails CodeWalks Boolean -> Boolean
;; Returns true if cws is a valid schedule according to the given
;; student preferences.

;; Example:

(begin-for-test
  (check-true
   (schedule/unavail-ok? (list ST1 ST2 ST3 ST4 ST5 ST6)  
                         (schedule/unavail (list ST1 ST2 ST3 ST4 ST5 ST6) CWS1))
   "same number of student" ))

;; Strategy: Function Composition

(define (schedule/unavail-ok? students cws)
  (schedule/general-ok? students cws false))

;;------------------------------------------------------------------------

;;Testing

;;------------------------------------------------------------------------

;Test for schedule/unavail

(begin-for-test
  (check-equal?   
   (schedule/unavail (list ST1 ST2 ST3) CWS0)
   CWS-TEST
   "Trivial Allocation")
  
  (check-equal?   
   (schedule/unavail (list ST1 ST2 ST3 ST4 ST5 ST6) CWS0)
   CWS-TEST1
   "equal: number of student, number of slots")
  
  (check-equal?   
   (schedule/unavail (list ST1 ST2 ST3 ST4 ST5 ST7) CWS2)
   CWS2-TEST
   "ST7 is never busy")
  
  (check-false   
   (schedule/unavail (list ST1 ST2 ST3 ST4 ST8 ST7) CWS2)
   "ST8 is always busy")
  
  (check-equal?   
   (schedule/unavail (list ST1 ST2 ST3 ST4 ST9 ST7) CWS2)
   CWS-BT
   "ST1 is back tracked")
  
  (check-false   
   (schedule/unavail (list ST1 ST2 ST3 ST4 ST9 ST7) CWS11)
   "slot is less than number of students")
  
  (check-equal?    
   (schedule/unavail (list ST10 ST11 ST12 ST7) CWS11)
   CWS-BT2
   "ST10 backtracked twice")
  
  (check-false    
   (schedule/unavail (list ST11 ST13) CWS11)
   "two students for slot"))

;;------------------------------------------------------------------------

;test for schedule/unavail-ok?

(begin-for-test
  (check-true   
   (schedule/unavail-ok? (list ST1 ST2 ST3 ST4 ST5 ST6)  
                         (schedule/unavail 
                          (list ST1 ST2 ST3 ST4 ST5 ST6) CWS1))
   "same number of student" )
  
  (check-false   
   (schedule/unavail-ok? (list ST1 ST2 ST3 ST4 ST5 ST6)  
                         (schedule/unavail 
                          (list ST1 ST2 ST3 ST6) CWS1))
   "same number of student" )
  
  (check-false   
   (schedule/unavail-ok? (list ST1 ST2 ST5 ST6)  
                         (schedule/unavail 
                          (list ST1 ST2 ST3 ST4 ST5 ST6) CWS1))
   "CodeWalk has ghost students " )
  
  (check-false   
   (schedule/unavail-ok? (list ST1 ST2 ST3 ST4 ST5 ST6 ST9)  
                         (schedule/unavail 
                          (list ST1 ST2 ST3 ST4 ST5 ST6) CWS1))
   "ST9 no in CodeWalks" )
  
  (check-false   
   (schedule/unavail-ok? (list ST1 ST2 ST3 )  
                         (schedule/unavail 
                          (list ST1 ST2 ST3 ST3 ) CWS1))
   "student is repeated" )
  
  (check-false   
   (schedule/unavail-ok? (list ST1 ST2 ST3 ST3 )  
                         (schedule/unavail 
                          (list ST1 ST2 ST3 ) CWS1))
   "list of student is not unique" )
  
  (check-false   
   (schedule/unavail-ok? (list ST11 ST13)  
                         empty)
   "when CodeWalk is erroneous "))

;;------------------------------------------------------------------------