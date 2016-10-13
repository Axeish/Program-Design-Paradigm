;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-with-avail) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 10 : sched-with-avail.rkt

;; Required Libraries
;;------------------------------------------------------------------------

(require "extras.rkt")
(require rackunit)
(require "sched-general.rkt")

;; Provide Functions
;;------------------------------------------------------------------------

(provide schedule/avail-ok?)
(provide schedule/avail)
(provide avg-choice)

;; Global Constants
;;------------------------------------------------------------------------

(define ZERO 0)
(define ONE 1)


;; Data Definitions
;;------------------------------------------------------------------------

;;; All Data Definition defined in sched-general.rkt

;;------------------------------------------------------------------------

;; Functions

;;---------------------------------------------------------------------------
;; schedule/avail : StudentAvails CodeWalks -> Maybe<CodeWalks>
;; Creates a codewalk schedule by assigning students to cws, 
;; while satisfying students' constraints.
;; Returns #f if no schedule is possible.

;; Example:

(begin-for-test
  (check-equal?
   (schedule/avail (list ST1 ST2 ST3) CWS1)
   CWS-A1
   "Trivial Allocation"))

;; Strategy: Function Composition

(define (schedule/avail students0 cws)
  (schedule/general students0 cws true))

;;---------------------------------------------------------------------------
;; schedule/avail-ok? : StudentAvails CodeWalks Boolean -> Boolean
;; Returns true if cws is a valid schedule according to the given
;; student preferences.

;; Example

(begin-for-test
  (check-true
   (schedule/avail-ok? (list ST1 ST2 ST3 ST12)  
                       (schedule/avail (list ST1 ST2 ST3 ST12) CWS11))
   "same number of student" ))

;; Strategy: Function Composition

(define (schedule/avail-ok? students cws)
  (schedule/general-ok? students cws true))

;;---------------------------------------------------------------------------

;; avg-choice : StudentAvails CodeWalks -> PosReal
;; Computes the avg of the choice preference of all the students
;; WHERE: (schedule/avail-ok? students cws) = #t

;; Example

(begin-for-test
  (check-equal? 
   (avg-choice (list ST1 ST2 ST3 ST12)  
               (schedule/avail (list ST1 ST2 ST3 ST12) CWS11))
   1.5
   "prefernece second"))

;; Strategy : Function comosition

(define (avg-choice students cws)
  (local
    (
     ; get-rank : StudentAvail -> PosReal
     ; Gets the rank of a student in the codewalk
     ; Strategy : Data decomposition on stdnt : StudentAvail
     (define (get-rank stdnt)
       (list-index-of
        (student-prefs stdnt)
        (get-cw-time
         (get-cw-for-stdnt (student-id stdnt) cws)))))
    
    ; -IN-
    (avg-int-list (map get-rank students))))

;;---------------------------------------------------------------------------

;; avg-int-list : ListOf<PosReal> -> PosReal
;; Computes avg of a list with pos real numbers

;; example

(begin-for-test
  (check-equal?
   (avg-int-list '(2 3 1 1 2 ))
   1.8
   "average is 1.8"))

;; Strategy : Function Composition

(define (avg-int-list list)
  (/ (foldr + ZERO list) (length list)))

;;---------------------------------------------------------------------------
;; get-cw-time : Codewalk -> Time
;; Helper function to get time from the codewalk

;; Example

(begin-for-test
  (check-equal?
   (get-cw-time CW2-TUE600 )
   TUE600
   "codewalk 6:00pmTues"))

;; Strategy : Data decomposition on cs : Codewalk

(define (get-cw-time cw)
  (codewalk-time cw))

;;---------------------------------------------------------------------------

;; list-index-of : List<Time> Time -> Integer
;; Gets the index of the given time in the list of times if found else
;; return -1

;; Example

(begin-for-test
  (check-equal?
   (list-index-of PREF-TAL TUE135)
   1
   "First codewalk")
  (check-equal?
   (list-index-of PREF-TAL TUE600)
   3
   "third codewalk")
  (check-equal?
   (list-index-of '() TUE135)
   0
   "Empty Preference"))

;; Strategy : Fucntion Composition

(define (list-index-of times0 tme)
  (local ( 
          ; List<Time> Integer -> List<Time>
          ; Returns the index of time if present, else -1
          ; WHERE: index-so-far is the index of times0
          ; so far and times is the list that has to be 
          ; processed.
          ; Strategy : Data decomposition on times : ListOf<Time>
          (define (index-of/a times index-so-far)
            (cond
              [(empty? times) ZERO]
              [else 
               (if (symbol=? tme (first times))
                   index-so-far
                   (index-of/a (rest times) (add1 index-so-far)))])))
    
    ; - IN -
    (index-of/a times0 ONE)))

;;---------------------------------------------------------------------------

;; get-cw-for-stdnt : StudentID Codewalks -> Maybe<Codewalk>
;; Returns a codewalk where studentid is present
;; WHERE StudentID definitely exists in one of the codewalks

;; Example

(begin-for-test
  (check-equal?
   (get-cw-for-stdnt 'Ashish (schedule/avail (list ST1 ST2 ST3) CWS1))
   (make-codewalk '1:35pmTues (list 'Ashish 'Kumar) 3)
   "Ashish is in first codewalk")
  (check-false
   (get-cw-for-stdnt 'ashish (schedule/avail (list ST1 ST2 ST3) CWS1))
   "ashish does not exist "))

;;Strategy : Function Composition

(define (get-cw-for-stdnt stid cws)
  (local (
          ; result/#f : Codewalks -> Codewalk
          ; If the returned codewalks are empty returns false else returns
          ; the first codewalk
          ; Strategy : Data decomposition on lst : Codewalks
          (define (result/#f lst)
            (cond
              [(empty? lst) #f]
              [else (first lst)]))
          
          ; filter-cws : -> Maybe<CodeWalk>
          ; Filters all the results that dont match the student id
          ; Strategy : Function composition
          (define filter-cws (filter     
                              ; Codewalk -> Maybe<CodeWalk>
                              ; Gets codewalk in which the student
                              ; is present
                              ; Strategy : Data decomposition on cw : Codewalk
                              (lambda (cw)
                                (member? stid (codewalk-students cw)))
                              cws)))
    ; -IN-
    (if (false? (result/#f filter-cws))
        #f
        (first filter-cws))))

;;---------------------------------------------------------------------------

;; Testing

;;---------------------------------------------------------------------------

;; Testing for schedule/avail

(begin-for-test
  (check-equal?   (schedule/avail (list ST1 ST2 ST3) CWS1)
                  CWS-A1
                  "Trivial Allocation")
  
  (check-equal?   (schedule/avail (list ST1 ST2 ST3 ST12) CWS11)
                  CWS-A2
                  "equal: number of student, number of slots")
  
  (check-equal?   (schedule/avail (list ST1 ST2 ST3 ST8) CWS11)
                  CWS-A3
                  "ST8 is never busy")
  
  (check-false   (schedule/avail (list ST1 ST2 ST3 ST7) CWS11)
                 "ST7 is always busy")
  
  (check-equal?   (schedule/avail (list ST2 ST10 ST4) CWS11)
                  CWS-TEST-AVAIL1
                  "ST2 is back tracked")
  
  (check-false   (schedule/avail (list ST1 ST2 ST3 ST4 ST9 ST7) CWS11)
                 "slot is less than number of students")
  
  (check-equal?   (schedule/avail (list ST8   ST4 ST10) CWS11)
                  CWS-TEST-AVAIL2
                  "ST8 backtracked twice")
  
  (check-equal?   (schedule/avail (list ST8   ST4 ST10 ST17 ) CWS11)
                  CWS-TEST-AVAIL3
                  "ST8 backtracked thrice")
  
  (check-false   (schedule/avail (list ST1 ST2 ST9) CWS11)
                 "two students for slot"))


;; Testing for schedule/avail-ok?

(begin-for-test
  (check-true   (schedule/avail-ok? (list ST1 ST2 ST3 ST12)  
                                    (schedule/avail 
                                     (list ST1 ST2 ST3 ST12) CWS11))
                "same number of student")
  
  (check-false   (schedule/avail-ok? (list ST1 ST2 ST3 ST12)  
                                     (schedule/avail 
                                      (list ST1 ST2 ST3) CWS11))
                 "students not assigned Codewalk")
  
  (check-false   (schedule/avail-ok? (list ST8 ST4)  
                                     (schedule/avail 
                                      (list ST8 ST4 ST10 ST17 ) CWS11))
                 "codewalk has ghost students")
  
  (check-false   (schedule/avail-ok? (list ST8 ST4 ST10 )  
                                     (schedule/avail 
                                      (list ST8 ST4 ST10 ST8 ) CWS11))
                 "student is repeated")
  
  (check-false   (schedule/avail-ok? (list ST8 ST4 ST8 ST10 )  
                                     (schedule/avail 
                                      (list ST8 ST4 ST10 ) CWS11))
                 "list of student is not unique"))

;; Testing for avg-choice

(begin-for-test
  (check-equal?    (avg-choice (list ST1 ST2 ST3 ST12)  
                               (schedule/avail (list ST1 ST2 ST3 ST12) CWS11))
                   1.5
                   "prefernece second")
  
  (check-equal?    (avg-choice (list ST1  ST3 ST17)  
                               (schedule/avail (list ST1  ST3 ST17) CWS11))
                   1
                   "preference first")
  
  (check-equal?    (avg-choice (list ST1 ST20 ST3 ST17)  
                               (schedule/avail (list ST20 ST1 ST3 ST17) CWS11))
                   1
                   "preference first all slots full")
  
  (check-equal?    (avg-choice (list ST8   ST4 ST10 ST17 )  
                               (schedule/avail (list ST8 ST4 ST10 ST17 ) CWS11))
                   1.75
                   "preference third for st8")
  
  (check-equal?    (avg-choice (list ST8   ST4 ST10)  
                               (schedule/avail (list ST8 ST4 ST10) CWS11))
                   5/3
                   "preference third for st8"))
;;---------------------------------------------------------------------------
