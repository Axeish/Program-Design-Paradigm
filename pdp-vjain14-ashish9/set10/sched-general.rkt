;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sched-general) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Problem Set 10 : sched-general.rkt

;; Required Libraries
;;------------------------------------------------------------------------

(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 13) ; hours spent

;; Provide Functions
;;------------------------------------------------------------------------

(provide (all-defined-out))

;; Constant 
;;-------------------------------------------------------------------------
(define MT-LS '())

;; Data Definitions
;;------------------------------------------------------------------------

;; A Time is a Symbol
;; Represents a day of week and time.

;; Example

(define TUE135 '1:35pmTues)
(define TUE325 '3:25pmTues)
(define TUE600 '6:00pmTues)
(define WED1145 '11:45amWed)
(define WED135 '1:35pmWed)
(define WED325 '3:25pmWed)


;;------------------------------------------------------------------------

; A StudentID is a Symbol
; Represents a student (or pair) via their ccs ID(s).

; Example
 (define EX-ST1 'vardhman)
 (define EX-ST2 'ashish)


;;------------------------------------------------------------------------

; A ListOf<StudentID> is one of:
; - empty
; - (cons StudentID ListOf<StudentID>)

;; Example
 (define EX-ST-LS1 (list EX-ST1))
 (define EX-ST-LS2 (list EX-ST1 EX-ST2))


;; Template for  ListOf<StudentID> 

;; studentid-ls-fn : ListOf<StudentID> -> ???
;; (define (studentid-ls-fn ls)
;;   (cond
;;     [(empty? ls) ...]
;;     [else (...(first ls)...(studentid-ls-fn (rest ls)))]))

;;------------------------------------------------------------------------

; A CodeWalk is a (make-codewalk Time ListOf<StudentID> PosInt)
; Represents a codewalk with time, assigned students, and a max capacity.

(define-struct codewalk (time students max))

;; Example

(define CW-TUE135 (make-codewalk TUE135 empty 3))
(define CW-TUE325 (make-codewalk TUE325 empty 3))
(define CW-TUE600 (make-codewalk TUE600 empty 3))
(define CW-WED1145 (make-codewalk WED1145 empty 3))
(define CW-WED135 (make-codewalk WED135 empty 3))
(define CW-WED325 (make-codewalk WED325 empty 3))
(define CW2-TUE135 (make-codewalk TUE135 empty 2))
(define CW2-TUE325 (make-codewalk TUE325 empty 2))
(define CW2-TUE600 (make-codewalk TUE600 empty 2))
(define CW1-TUE135 (make-codewalk TUE135 empty 1))
(define CW1-TUE325 (make-codewalk TUE325 empty 1))
(define CW1-TUE600 (make-codewalk TUE600 empty 1))
(define CW1-WED1145 (make-codewalk WED1145 empty 1))
(define CW1-WED135 (make-codewalk WED135 empty 1))
(define CW1-WED325 (make-codewalk WED325 empty 1))

;; Template
;; codewalk-fn : CodeWalk -> ???
; (define (codewalk-fn cw)
;   (... (codewalk-time cw)...
;        (studentid-ls-fn (codewalk-students cw))...
;        (codewalk-max cw)...))

;;------------------------------------------------------------------------

;; A CodeWalks is a ListOf<CodeWalk>
;; Represents a codewalk schedule.

;; Example

(define CWS1 (list 
              CW-TUE135
              CW-TUE325
              CW-TUE600))
(define CWS0 (list 
              CW1-TUE135
              CW1-TUE325
              CW1-TUE600
              CW1-WED1145
              CW1-WED135
              CW1-WED325))
(define CWS2 (list 
              CW2-TUE135
              CW2-TUE325
              CW2-TUE600))
(define CWS11 (list 
              CW1-TUE135
              CW1-TUE325
              CW1-TUE600
              CW1-WED1145))
(define CWS-ONE (list 
                CW1-TUE135))

(define CWS-TEST(list
    (make-codewalk '1:35pmTues (list 'Vardhman) 1)
    (make-codewalk '3:25pmTues (list 'Ashish) 1)
    (make-codewalk '6:00pmTues empty 1)
    (make-codewalk '11:45amWed (list 'Kumar) 1)
    (make-codewalk '1:35pmWed empty 1)
    (make-codewalk '3:25pmWed empty 1)))

(define CWS-TEST1 (list
 (make-codewalk '1:35pmTues (list 'Vardhman) 1)
 (make-codewalk '3:25pmTues (list 'Ashish) 1)
 (make-codewalk '6:00pmTues (list 'Jain) 1)
 (make-codewalk '11:45amWed (list 'Kumar) 1)
 (make-codewalk '1:35pmWed (list 'Steven) 1)
 (make-codewalk '3:25pmWed (list 'Vijay) 1)))

(define CWS2-TEST (list
 (make-codewalk '1:35pmTues (list 'Vardhman 'Brian) 2)
 (make-codewalk '3:25pmTues (list 'Ashish 'Kumar) 2)
 (make-codewalk '6:00pmTues (list 'Jain 'Steven) 2)))

(define CWS-BT (list
 (make-codewalk '1:35pmTues (list 'Vardhman 'Brian) 2)
 (make-codewalk '3:25pmTues (list 'Kumar 'Snake) 2)
 (make-codewalk '6:00pmTues (list 'Ashish 'Jain) 2)))

(define CWS-BT2 (list
 (make-codewalk '1:35pmTues (list 'Sheen) 1)
 (make-codewalk '3:25pmTues (list 'Brian) 1)
 (make-codewalk '6:00pmTues (list 'David) 1)
 (make-codewalk '11:45amWed (list 'Brady) 1)))

(define CWS-SPACE (list
 (make-codewalk '1:35pmTues (list 'Sheen) 2)
 (make-codewalk '3:25pmTues (list 'Brian) 1)
 (make-codewalk '6:00pmTues (list 'David) 1)
 (make-codewalk '11:45amWed (list 'Brady) 1)))


(define CWS-SPACE-FILLED (list
 (make-codewalk '1:35pmTues (list 'Sheen 'Ashish) 2)
 (make-codewalk '3:25pmTues (list 'Brian) 1)
 (make-codewalk '6:00pmTues (list 'David) 1)
 (make-codewalk '11:45amWed (list 'Brady) 1)))

(define CWS-A1 (list
    (make-codewalk '1:35pmTues (list 'Ashish 'Kumar) 3)
    (make-codewalk '3:25pmTues (list 'Vardhman) 3)
    (make-codewalk '6:00pmTues empty 3)))
(define CWS-A2 (list
    (make-codewalk '1:35pmTues (list 'Ashish) 1)
    (make-codewalk '3:25pmTues (list 'Vardhman) 1)
    (make-codewalk '6:00pmTues (list 'Kumar) 1)
    (make-codewalk '11:45amWed (list 'David) 1)))

(define CWS-A3
  (list
    (make-codewalk '1:35pmTues (list 'Ashish) 1)
    (make-codewalk '3:25pmTues (list 'Vardhman) 1)
    (make-codewalk '6:00pmTues (list 'Kumar) 1)
    (make-codewalk '11:45amWed (list 'Joy) 1)))

(define CWS-TEST-AVAIL1 (list
    (make-codewalk '1:35pmTues (list 'Jain) 1)
    (make-codewalk '3:25pmTues (list 'Brady) 1)
    (make-codewalk '6:00pmTues (list 'Kumar) 1)
    (make-codewalk '11:45amWed empty 1)))

(define CWS-TEST-AVAIL2
  (list
    (make-codewalk '1:35pmTues (list 'Jain) 1)
    (make-codewalk '3:25pmTues (list 'Brady) 1)
    (make-codewalk '6:00pmTues (list 'Joy) 1)
    (make-codewalk '11:45amWed empty 1)))
(define CWS-TEST-AVAIL3
  (list
    (make-codewalk '1:35pmTues (list 'Jain) 1)
    (make-codewalk '3:25pmTues (list 'Brady) 1)
    (make-codewalk '6:00pmTues (list 'Askol) 1)
    (make-codewalk '11:45amWed (list 'Joy) 1)))

; TEMPLATE:-
; codewalks-fn : ListOf<Codewalk> -> ???
;(define (codewalks-fn cws)
;  (cond
;	[(empty? cws) ...]
;	[else (... (codewalk-fn (first cws)) ...
;           	(codewalks-fn (rest cws)) ...)]))


;;------------------------------------------------------------------------

; A Preferences is a ListOf<Time>
; Represents a list of code walk times.

 ;;Example

(define PREF-T1 (list TUE135))
(define PREF-T13 (list TUE135 TUE600))
(define PREF-T23 (list TUE325 TUE600))
(define PREF-T21 (list TUE325 TUE135))
(define PREF-T0 MT-LS)
(define PREF-TAL (list TUE135 TUE325 TUE600 WED1145 WED135 WED325))
(define PREF-T2 (list TUE325))
(define PREF-T234 (list TUE325 TUE600 WED1145))
(define PREF-T24 (list TUE325 WED1145 ))
(define PREF-T2345 (list TUE325 TUE600 WED1145 WED135))
(define PREF-T356 (list TUE600 WED135 WED325))
(define PREF-T41 (list WED1145 TUE135))

;; Template  
;; preferences-fn : ListOf<Time> -> ???
;; (define (preferences-fn prefs)
;;   (cond
;;     [(empty? prefs) ...]
;;     [else (...(first prefs)...(preferences-fn (rest prefs)))]))


;;------------------------------------------------------------------------ 

; A StudentUnavail is a (make-student StudentID Preferences)
; Represents a student and their unavailable times

(define-struct student (id prefs))

;; Example 

(define ST1 (make-student 'Ashish PREF-T1))
(define ST2 (make-student 'Kumar PREF-T13))
(define ST3 (make-student 'Vardhman PREF-T23))
(define ST4 (make-student 'Jain PREF-T1))
(define ST5 (make-student 'Steven PREF-T21))
(define ST6 (make-student 'Vijay PREF-T23))
(define ST7 (make-student 'Brian PREF-T0))
(define ST8 (make-student 'Joy PREF-TAL))
(define ST9 (make-student 'Snake PREF-T13))
(define ST10 (make-student 'Brady PREF-T2))
(define ST11 (make-student 'Sheen PREF-T234))
(define ST12 (make-student 'David PREF-T24))
(define ST13 (make-student 'Danielle PREF-T234))
(define PT14 (make-student 'Vjain14-Ashish9 PREF-T2345))
(define PT15 (make-student 'VijayP-Ashish9 PREF-T23))
(define ST16 (make-student 'Ashish PREF-T2))
(define ST17 (make-student 'Askol PREF-T356))
(define ST20 (make-student 'Shankar PREF-T41))

;; Template-
; st-unv-fn : StudentUnavail -> ???
;(define (st-unv-fn s)
;  (...
;   (student-id s) ...
;   (preferences-fn (student-prefs s)) ...))
;       

;;------------------------------------------------------------------------  

; A StudentAvail is a (make-student StudentID Preferences)
; Represents a student and their available times, most-preferred first.
; An unlisted time means the student is unavailable.

;; Examples:

 (define ST-LS-AV1 (make-student 'Ashish (list TUE135 TUE325)))
 (define ST-LS-AV2 (make-student 'Vardhman (list TUE135 WED325 TUE600)))

;; Template
; st-av-fn : StudentAvail -> ???
;(define (st-av-fn s)
;  (...
;   (student-id s) ...
;   (preferences-fn (student-prefs s)) ...))


;;------------------------------------------------------------------------ 
; A StudentUnavails is a ListOf<StudentUnavail>

;; Examples:

 (define ST-LS-UNV1 (list ST1))
 (define ST-LS-UNV2 (list ST1 ST2))

;; Template
; st-unvs-fn : ListOf<StudentUnavail> -> ???
;(define (st-unvs-fn stunvs)
;  (cond
;	[(empty? stunvs) ...]
;	[else (... (st-unv-fn (first stunvs)) ...
;           	(st-unvs-fn (rest stunvs)) ...)]))

;;------------------------------------------------------------------------ 

; A Student is one of:
; - StudentUnavail
; - StudentAvail
; WHERE: there are no duplicate StudentIDs

;;------------------------------------------------------------------------  
; A StudentAvails is a ListOf<StudentAvail>

;; Examples:

 (define ST-AV1 (list ST1))
 (define ST-AV2 (list ST1 ST2))

;; Template
; st-avs-fn : ListOf<StudentAvail> -> ???
;(define (st-avs-fn stavs)
;  (cond
;	[(empty? avs) ...]
;	[else (... (st-av-fn (first stavs)) ...
;           	(st-avs-fn (rest stavs)) ...)]))



;;------------------------------------------------------------------------  

; A Students is a ListOf<Student>
; WHERE: there are no duplicate StudentIDs

;; Examples:

(define ST-LS-1 (list ST1))
(define ST-LS-2 (list ST1 ST2))

;; Functions
;;------------------------------------------------------------------------ 

; A CompOp is one of:
; <
; <=
; Represents < and <= comparator operators
; Examples;
 (define AOP1 <)
 (define AOP2 <=)
;;------------------------------------------------------------------------ 

;;------------------------------------------------------------------------ 
; Functions
;;------------------------------------------------------------------------ 

; schedule/general-ok? : Students CodeWalks Boolean -> Boolean
; Returns true if cws is a valid schedule according to the given
; student preferences.
;; Example
(begin-for-test
  (check-true
   (schedule/general-ok? (list ST1 ST2 ST3 ST4 ST5 ST6)  
   CWS-TEST1 false)
   "Student according to unavail preference" ))

;; Strategy: Function Composition 
(define (schedule/general-ok? students cws avail?)
  (and
      (validate-all-capacity cws)
      (validate-students cws students)
      (validate-student-time-pref cws students avail?)))

;;-------------------------------------------------------------------
; validate-all-capacity : CodeWalks -> Boolean
; Checks if the number of students in the codewalk
; don't exceed the maximum number of possible students
;; Example
(begin-for-test
  (check-true
   (validate-all-capacity CWS-TEST1)
   "Validate Capacity" ))


; Strategy : Function Composition
(define (validate-all-capacity cws)
  (andmap (
           ; CodeWalk -> Boolean
           ; Checks if length of students in the given codewalk
           ; is not greater than the max allowed
           ; Strategy : Data decomposition on cw : Codewalk
           lambda (cw) 
            (validate-cw-capacity cw <=)) cws))

;;-------------------------------------------------------------------
; validate-cw-capacity : CodeWalk Compop -> Boolean
; Checks if the number of students in given codewalk are
; < or <= to the max capacity on the basis of given op
;Example
(begin-for-test
  (check-true
   (validate-cw-capacity 
    (make-codewalk '1:35pmTues(list 'Vardhman 'Brian) 2) <=)
   "Validate Capacity" )
  (check-false
  (validate-cw-capacity 
    (make-codewalk '1:35pmTues(list 'Vardhman 'Brian) 1) <=)
   "Invalid Capacity" ))
; Strategy : Data  Decomposition on cw : CodeWalk
(define (validate-cw-capacity cw op)
  (op (length (codewalk-students cw)) (codewalk-max cw)))

;;-------------------------------------------------------------------
; validate-students : CodeWalks Students  -> Boolean
; Validates if all the students in the codewalks get unique slot and
; every student is present at least once in the codewalks
; Examples
(begin-for-test
  (check-true
   (validate-students 
    (schedule/general (list ST1 ST2 ST3 ST4 ST5 ST6) CWS1 false)
    (list ST1 ST2 ST3 ST4 ST5 ST6))
   "valid codewalk" )
  (check-false
  (validate-students
   (schedule/general (list ST1 ST2 ST3 ST6) CWS11 false)
   (list ST1 ST2 ST3))
   "Invalid codewalk" ))

; Strategy : Function Composition
(define (validate-students cws students)
  (cmpr-students-all-cws (all-stid-from-cws cws)
                       (all-stid-from-students students)))


;;-------------------------------------------------------------------
; all-stid-from-cws : CodeWalks -> ListOf<StudentID>
; Gets all the students from the list of all the codewalks
; Examples
(begin-for-test
  (check-equal?
  (all-stid-from-cws CWS-TEST1)
  (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
   "List of students returned" ))
; Strategy : Function Composition
(define (all-stid-from-cws cws)
  (foldr ( 
          ; CodeWalk ListOf<StudentID> -> ListOf<StudentID>
          ; Gets all the students from the list of all the codewalks
          ; Strategy : Data decomposition on cw : Codewalk
          lambda (cw stids)
           (append (codewalk-students cw) 
                   stids)) 
        MT-LS 
        cws))
;;-------------------------------------------------------------------

; all-stid-from-students : Students  -> ListOf<StudentID>
; Gets all the student ids from the list of all the students
; Examples:
(begin-for-test
  (check-equal?
  (all-stid-from-students (list ST3 ST1 ST2 ST4 ST5 ST6))
  (list 'Vardhman 'Ashish 'Kumar 'Jain 'Steven 'Vijay)
   "List of students returned" ))
; Strategy : Function Composition
(define (all-stid-from-students students)
  (foldr ( 
          ; Students ListOf<StudentID> -> ListOf<StudentID>
          ; Gets all the student ids from the list of all the students
          ; Strategy : Data decomposition on  : Codewalk
          lambda (studnt stids)
           (cons (student-id studnt) 
                   stids)) 
        MT-LS 
        students))

;;-------------------------------------------------------------------
; cmpr-students-all-cws : ListOf<StudentID> ListOf<StudentID> -> Boolean
; Validates if all the students in the codewalks get unique slot and
; every student is present at least once in the codewalks
; Examples
(begin-for-test
  (check-false
  (cmpr-students-all-cws (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                      (list 'Vardhman 'Ashish 'Jain 'Kumar))
   "not valid compare" )
  
  (check-true
  (cmpr-students-all-cws (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                      (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "valid compare" )
  
  (check-false
  (cmpr-students-all-cws (list 'Vardhman 'Ashish 'Jain 'Jain 'Steven 'Vijay)
                      (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "valid compare" ))
; Strategy : Function Composition
(define (cmpr-students-all-cws cws-stids all-stids)
  (and (validate-student-uniqueness cws-stids)
       (lst-length-equal? cws-stids all-stids)
       (all-elems-equal? cws-stids all-stids)))

;;-------------------------------------------------------------------
; validate-student-uniqueness : ListOf<StudentID> -> Boolean
; Checks if a student exists only once in the given code walk
; schedules
; Examples
(begin-for-test
  (check-true
  (validate-student-uniqueness
   (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "all unique" )
  (check-true
  (validate-student-uniqueness empty)
   "all unique" )
  (check-false
  (validate-student-uniqueness 
   (list 'Vardhman 'Ashish 'Jain 'Kumar 'Jain 'Vijay))
   "all unique" ))

; Strategy : Generative Recursion
(define (validate-student-uniqueness cws-stids)
  (cond
       [(empty? cws-stids) true]
       [(member? (first cws-stids) (rest cws-stids)) false]
       [else (validate-student-uniqueness (rest cws-stids))]))

;;-------------------------------------------------------------------

; all-elems-equal? : ListOf<StudentID> ListOf<StudentID> -> Boolean
; Checks if all the items in the first list are present in the
; second list
; Examples
(begin-for-test
  (check-false
  (all-elems-equal? (list 'Vardhman 'Ashish 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar))
   "all not present" )
  
  (check-true
  (all-elems-equal? (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "all present" ))
; Strategy : Function Composition
(define (all-elems-equal? lst1 lst2)
  (andmap (
           ; X -> Boolean
           ; Checks if current item of the first list are present in the
           ; second list
           ; Strategy : Function Composition
           lambda (ls2)
            (ormap (
                    ; X -> Boolean
                    ; Checks if current item of the first list is equal to 
                    ; the item being iterated in the second list
                    ; Strategy : Function Composition
                    lambda (ls1)
                    (symbol=? ls1 ls2)) lst1)) lst2))

;;-------------------------------------------------------------------
; lst-length-equal? : ListOf<X> ListOf<X> -> Boolean
; Checks if the length of the two given lists lst1 and lst2 are
; equal or not
; Examples
(begin-for-test
  (check-false
  (lst-length-equal? (list 'Vardhman 'Ashish 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar))
   "all not present" )
  
  (check-true
  (lst-length-equal? (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "all present" ))
; Strategy : Function Composition
(define (lst-length-equal? lst1 lst2)
  (= (length lst1) (length lst2)))


;;-------------------------------------------------------------------
; validate-student-time-pref : CodeWalks Students Boolean -> Boolean
; Validates if the alloted time slots to all the students dont fall
; in the unavailable preferences given by the students
; Examples
(begin-for-test
  (check-false
  (validate-student-time-pref CWS-BT2 (list ST7 ST10 ST11 ST12) true)
   "prefs not alloted" )
  
  (check-true
  (validate-student-time-pref CWS-BT2 (list ST7 ST10 ST11 ST12) false)
   "prefs alloted" ))
; Strategy : Function Composition
(define (validate-student-time-pref cws students avail?)
  (andmap (
           ; StudentUnavail -> Boolean
           ; Validates if the alloted time slots for the given student
           ; does not fall in the unavailable preferences
           ; Strategy : Data decomposition on stdnt : Student
           lambda (stdnt)
               (boolean=? (cmpr-cw-stdnt-pref 
                           (get-first 
                            (find-matching-student-cws cws 
                                                  (student-id stdnt)))
                           (student-prefs stdnt))
                         avail?)) 
            students))


;;-------------------------------------------------------------------
; find-matching-student-cws : CodeWalks StudentID -> CodeWalks
; Finds the codewalk the given student has been assigned to
; Examples
(begin-for-test
  (check-equal?
  (find-matching-student-cws CWS-BT2 'Sheen)
  (list (make-codewalk '1:35pmTues (list 'Sheen) 1))
   "found codewalk" ))
; Strategy : Function Composition
(define (find-matching-student-cws cws stid)
     (filter (
              ; CodeWalk -> CodeWalk
              ; Checks is the student has been assigned the given codewalk
              ; or not
              ; Strategy : Data decomposition on cw : Codewalk
              lambda (cw)
               (member? stid (codewalk-students cw))) cws))

;;-------------------------------------------------------------------
; cmpr-cw-stdnt-pref : Maybe<CodeWalk> Preferences -> Boolean
; Checks if the time slot of the given codewalk exists in the
; given list of preferences
; Examples
(begin-for-test
  (check-false
   (cmpr-cw-stdnt-pref 
     false
     (list (make-codewalk '1:35pmTues (list 'Sheen) 1)))
   "codewalk is false" ))
; Strategy : Data decomposition on prefs : Preferences
(define (cmpr-cw-stdnt-pref cw prefs)
  (if (false? cw)
      #f
      (member? (codewalk-time cw) prefs)))

;;-------------------------------------------------------------------
; get-first : ListOf<X> -> Maybe<X>
; Returns first item of list X
; Examples
(begin-for-test
  (check-equal?
   (get-first empty)
     #f
   "lst is empty" )
  
  (check-equal?
   (get-first (list ST1 ST2))
     ST1
   "first returned" ))
; Strategy : Data decomposition on lst : ListOf<X>
(define (get-first lst)
  (cond
      [(empty? lst) #f]
      [else (first lst)]))

;;-------------------------------------------------------------------
; schedule/general : Students CodeWalks Boolean -> Maybe<CodeWalks>
; Creates a codewalk schedule by assigning students to cws, 
; while satisfying students' constraints.
; Returns #f if no schedule is possible.
; Examples
; Strategy: Generative Recursion
; TERMINATION ARGUMENT:
; The function always terminates as the list of prefs after every
; unsuccessful find of a slot, goes smaller. Once all student prefs are
; exhausted, the function results false.
; HALTING MEASURE:
; - The list of students becomes empty
; Examples
(begin-for-test
  (check-equal?
    (schedule/general (list ST10 ST11 ST12 ST7) CWS11 false)
    CWS-BT2
   "ST10 backtracked twice")
   
   (check-false
    (schedule/general (list ST11 ST13) CWS11 false)
   "two students for slot"))
; Strategy: ???
(define (schedule/general students0 cws avail?)
  (local (
          ;; schedule/general/a : List<StudentsUnavail> List<Time> 
          ;;                          CodeWalks -> Maybe<Codewalks>
          ;; Returns all the def entities present in the program as a list
          ;; WHERE: cws-so-far is the list of codewalks that is being updated 
          ;; everytime a student is placed in the codewalks
          ;; Strategy : Generative Recursion
          ;; The function always terminates as the list of prefs after every
          ;; unsuccessful find of a slot, goes smaller. Once all student prefs 
          ;; exhausted, the function results false.
          ;; HALTING MEASURE:
          ;; - The list of students becomes empty
          ;; - The list of prefs become empty
          (define (schedule/general/a stdnts prefs cws-so-far)
            (cond
                 [(empty? stdnts) cws-so-far]
                 [(empty? prefs) #f]
                 [else 
                  (local (
                          (
                           ;; result/#f : -> Maybe<Codewalks>
                           ;; Returns false if all students are not palced in
                           ;; codewalks else codewalks
                           ;; Strategy : Function Composition
                           define result/#f 
                            (local
                              (
                               (
                                ;; backtrack-check/#f : -> Maybe<Codewalks>
                                ;; Called when backtracking is needed
                                ;; Strategy : 
                                ;; Data decomposition on stdnts : Students
                                define backtrack-check/#f 
                                 (schedule/general 
                                  (rest stdnts)                                                                
                                  (add-stdnt-to-cws 
                                   (allot-cw (first stdnts)
                                             cws-so-far
                                             (get-first prefs))
                                   (first stdnts)
                                   cws-so-far)
                                  avail?))
                               
                               (
                                ;; call-sched-acc : -> Maybe<Codewalks>
                                ;; Called when current student is alloted time,
                                ;; trying to alot time to next
                                ;; Strategy : 
                                ;;Data decomposition on prefs : ListOf<Time>
                                define call-sched-acc 
                                 (schedule/general/a 
                                  stdnts 
                                  (rest prefs)
                                  cws-so-far))
                               
                               (
                                ;; handle-non-allot : -> Maybe<Codewalks>
                                ;; Calls case where student
                                ;; was not alloted time
                                ;; and backtracking is needed
                                ;; Strategy : Function Composition
                                define handle-non-allot
                                 (if (false? backtrack-check/#f)
                                      call-sched-acc                                                                
                                      backtrack-check/#f)))
                              
                              (if (false? 
                                   (allot-cw 
                                    (first stdnts)
                                    cws-so-far
                                    (first prefs)))
                                  call-sched-acc
                                  handle-non-allot
                                  ))))
                    (if (false? result/#f)
                        #f
                        result/#f))])))
    
    (schedule/general/a students0 
                      (get-pref-top-stdnt students0 cws avail?)
                      cws))) 

;;-------------------------------------------------------------------

; add-stdnt-to-cws : Maybe<CodeWalk> Student CodeWalks -> CodeWalks
; Returns the list of codewalks after adding the given student to 
; given matching codewalk
; Examples
(begin-for-test
  (check-equal?
    (add-stdnt-to-cws (make-codewalk '1:35pmTues (list 'Sheen) 2)
                    ST1
                    CWS-SPACE)
    CWS-SPACE-FILLED
   "ST1 added to codewalk")
  
  (check-equal?
    (add-stdnt-to-cws #f
                      ST1
                    CWS-SPACE)
    CWS-SPACE
   "ST1 added to codewalk"))
; Strategy : Function composition
(define (add-stdnt-to-cws cw stdnt cws)
  (if (false? cw)
      cws
      (map (
        ; CodeWalk -> CodeWalks
        ; Returns the list of codewalks after adding the given student to 
        ; given matching codewalk
        ; Strategy : Data decomposition on cw, cw-itr : Codewalk
        lambda (cw-itr)
          (if (symbol=? (codewalk-time cw) (codewalk-time cw-itr))
              (make-codewalk (codewalk-time cw-itr)
                            (append (codewalk-students cw-itr)
                                   (list (get-stdnt-id stdnt)))
                            (codewalk-max cw-itr))
              cw-itr)) cws)))

;;-------------------------------------------------------------------
; get-stdnt-id : Student -> StudentID
; Gets the student id for the given student , this is a helper function
; Examples
(begin-for-test
  (check-equal?
    (get-stdnt-id ST10)
    'Brady
   "student id is Brady"))
; Strategy : Data decomposition on stdnt : Student
(define (get-stdnt-id stdnt)
  (student-id stdnt))

;;-------------------------------------------------------------------
; allot-cw : Student CodeWalks List<Time> -> Maybe<CodeWalk>
; Returns a codewalk if student can be alloted to one else returns false
; Examples
(begin-for-test
  (check-equal?
    (allot-cw ST1 CWS-SPACE TUE135)
    (make-codewalk '1:35pmTues (list 'Sheen) 2)
   "codewalk possible")
  
  (check-false
    (allot-cw ST1 CWS-SPACE WED1145)
   "codewalk not possible"))
; Strategy : Function composition
(define (allot-cw stdnt cws prefs)
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
          ; Filters all the results that dont match pref
          ; Strategy : Function composition
          (define filter-cws (filter (
                                   ; Codewalk -> Maybe<CodeWalk>
                                   ; Checks if the given codewalk is safe
                                   ; or not
                                   ; Strategy : Function composition
                                   lambda (cw)
                                    (safe-add-stdnt-cw? (list prefs)
                                                      cw)) cws)))
    
    (if (false? (result/#f filter-cws))
        #f
        (first filter-cws))))


;;-------------------------------------------------------------------
; get-pref-top-stdnt : Students CodeWalks Boolean -> List<Time>
; Gets all timing prefs that are not in the given
; unavailable prefs of the student
; Examples
(begin-for-test
  (check-equal?
    (get-pref-top-stdnt (list ST1 ST2) CWS-SPACE true)
    (list TUE135)
   "prefs returned")
  
  (check-equal?
    (get-pref-top-stdnt empty CWS-SPACE true)
    empty
   "prefs returned"))
; Strategy : Data decomposition on stds : Students
(define (get-pref-top-stdnt stds cws avail?)
  (cond
      [(empty? stds) empty]
      [else (get-stdnt-pref (first stds) cws avail?)]))

;;-------------------------------------------------------------------
; get-stdnt-pref : Students CodeWalks Boolean -> List<Time>
; Gets all timing prefs that are not in the given
; unavailable prefs of the student
(begin-for-test
  (check-equal?
    (get-stdnt-pref ST1 CWS-SPACE true)
    (list TUE135)
   "prefs returned")
  
  (check-equal?
    (get-stdnt-pref ST1 CWS-SPACE false)
    (list TUE325 TUE600 WED1145)
   "prefs returned"))
; Strategy : Data decomposition on stds : Students
(define (get-stdnt-pref stdnt cws avail?)
  (local(
         ; get-cws-time : List<Time> -> List<Time>
         ; Gets all timing prefs that are not in the given
         ; unavailable prefs of the student
         ; Strategy : Function Composition
        (define (get-top-stdnt-cws-time prefs)
         (map (
               ; Codewalk -> Time
               ; Returns the codewalk time for the given codewalk c
               ; Strategy : Data decomposition on c : Codewalk
               lambda (c)
                (codewalk-time c)) 
              
              (filter (
                       ; Codewalk -> Time
                       ; Returns the list of codewalks that match the 
                       ; given preference of time, would be one list or empty
                       ; Strategy : Data decomposition on cw : Codewalk
                       lambda (cw)
                             (boolean=? (member? (codewalk-time cw)
                                       prefs)
                                       avail?)) cws))))
     
  (get-top-stdnt-cws-time (student-prefs stdnt))))

;;-------------------------------------------------------------------

; safe-add-stdnt-cw? : List<Time> CodeWalk -> Boolean
; Checks if it is safe to add the given student pref to the given 
; codewalk
; Examples
(begin-for-test
  (check-equal?
    (safe-add-stdnt-cw? #f (make-codewalk '1:35pmTues (list 'Sheen) 2))
    #f
   "not safe to add")
  
  (check-equal?
    (safe-add-stdnt-cw? (list TUE135) 
                        (make-codewalk '1:35pmTues (list 'Sheen) 2))
    #t
   "safe to add"))

; Strategy : Function Composition
(define (safe-add-stdnt-cw? prefs cw)
  (if (false? prefs)
      #f
      (and (cmpr-cw-stdnt-pref cw prefs)
       (validate-cw-capacity cw <))))

;---------------------------------------------------------------------------

;; Testing
;---------------------------------------------------------------------------


; testing for validate-all-capacity

(begin-for-test
  (check-true
   (validate-all-capacity CWS-TEST1)
   "Validate Capacity" ))


; testing for validate-cw-capacity

(begin-for-test
  (check-true
   (validate-cw-capacity 
    (make-codewalk '1:35pmTues(list 'Vardhman 'Brian) 2) <=)
   "Validate Capacity" )
  (check-false
  (validate-cw-capacity 
    (make-codewalk '1:35pmTues(list 'Vardhman 'Brian) 1) <=)
   "Invalid Capacity" ))


; testing for validate-students

(begin-for-test
  (check-true
   (validate-students 
    (schedule/general (list ST1 ST2 ST3 ST4 ST5 ST6) CWS1 false)
    (list ST1 ST2 ST3 ST4 ST5 ST6))
   "valid codewalk" )
  (check-false
  (validate-students
   (schedule/general (list ST1 ST2 ST3 ST6) CWS11 false)
   (list ST1 ST2 ST3))
   "Invalid codewalk" ))


; testing for all-stid-from-cws

(begin-for-test
  (check-equal?
  (all-stid-from-cws CWS-TEST1)
  (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
   "List of students returned" ))


; testing for all-stid-from-students

(begin-for-test
  (check-equal?
  (all-stid-from-students (list ST3 ST1 ST2 ST4 ST5 ST6))
  (list 'Vardhman 'Ashish 'Kumar 'Jain 'Steven 'Vijay)
   "List of students returned" ))


; testing for cmpr-students-all-cws


(begin-for-test
  (check-false
  (cmpr-students-all-cws (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                      (list 'Vardhman 'Ashish 'Jain 'Kumar))
   "not valid compare" )
  
  (check-true
  (cmpr-students-all-cws (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                      (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "valid compare" )
  
  (check-false
  (cmpr-students-all-cws (list 'Vardhman 'Ashish 'Jain 'Jain 'Steven 'Vijay)
                      (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "valid compare" ))



; testing for validate-student-uniqueness

(begin-for-test
  (check-true
  (validate-student-uniqueness 
   (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "all unique" )
  (check-true
  (validate-student-uniqueness empty)
   "all unique" )
  (check-false
  (validate-student-uniqueness 
   (list 'Vardhman 'Ashish 'Jain 'Kumar 'Jain 'Vijay))
   "all unique" ))


; testing for all-elems-equal?


(begin-for-test
  (check-false
  (all-elems-equal? (list 'Vardhman 'Ashish 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar))
   "all not present" )
  
  (check-true
  (all-elems-equal? (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "all present" ))


; testing for lst-length-equal?


(begin-for-test
  (check-false
  (lst-length-equal? (list 'Vardhman 'Ashish 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar))
   "all not present" )
  
  (check-true
  (lst-length-equal? (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay)
                  (list 'Vardhman 'Ashish 'Jain 'Kumar 'Steven 'Vijay))
   "all present" ))


; testing for validate-student-time-pref


(begin-for-test
  (check-false
  (validate-student-time-pref CWS-BT2 (list ST7 ST10 ST11 ST12) true)
   "prefs not alloted" )
  
  (check-true
  (validate-student-time-pref CWS-BT2 (list ST7 ST10 ST11 ST12) false)
   "prefs alloted" ))


; testing for find-matching-student-cws

(begin-for-test
  (check-equal?
  (find-matching-student-cws CWS-BT2 'Sheen)
  (list (make-codewalk '1:35pmTues (list 'Sheen) 1))
   "found codewalk" ))


; testing for cmpr-cw-stdnt-pref

(begin-for-test
  (check-false
   (cmpr-cw-stdnt-pref 
     false
     (list (make-codewalk '1:35pmTues (list 'Sheen) 1)))
   "codewalk is false" ))


; testing for get-first

(begin-for-test
  (check-equal?
   (get-first empty)
     #f
   "lst is empty" )
  
  (check-equal?
   (get-first (list ST1 ST2))
     ST1
   "first returned" ))


; testing for schedule/general

(begin-for-test
  (check-equal?
    (schedule/general (list ST10 ST11 ST12 ST7) CWS11 false)
    CWS-BT2
   "ST10 backtracked twice")
   
   (check-false
    (schedule/general (list ST11 ST13) CWS11 false)
   "two students for slot"))


; testing for add-stdnt-to-cws

(begin-for-test
  (check-equal?
    (add-stdnt-to-cws (make-codewalk '1:35pmTues (list 'Sheen) 2)
                    ST1
                    CWS-SPACE)
    CWS-SPACE-FILLED
   "ST1 added to codewalk")
  
  (check-equal?
    (add-stdnt-to-cws #f
                    ST1
                    CWS-SPACE)
    CWS-SPACE
   "ST1 added to codewalk"))


; testing for get-stdnt-id

(begin-for-test
  (check-equal?
    (get-stdnt-id ST10)
    'Brady
   "student id is Brady"))


; testing for allot-cw


(begin-for-test
  (check-equal?
    (allot-cw ST1 CWS-SPACE TUE135)
    (make-codewalk '1:35pmTues (list 'Sheen) 2)
   "codewalk possible")
  
  (check-false
    (allot-cw ST1 CWS-SPACE WED1145)
   "codewalk not possible"))


; testing for get-pref-top-stdnt

(begin-for-test
  (check-equal?
    (get-pref-top-stdnt (list ST1 ST2) CWS-SPACE true)
    (list TUE135)
   "prefs returned")
  
  (check-equal?
    (get-pref-top-stdnt empty CWS-SPACE true)
    empty
   "prefs returned"))


; testing for get-stdnt-pref

(begin-for-test
  (check-equal?
    (get-stdnt-pref ST1 CWS-SPACE true)
    (list TUE135)
   "prefs returned")
  
  (check-equal?
    (get-stdnt-pref ST1 CWS-SPACE false)
    (list TUE325 TUE600 WED1145)
   "prefs returned"))


; testing for safe-add-stdnt-cw?

(begin-for-test
  (check-equal?
    (safe-add-stdnt-cw? #f (make-codewalk '1:35pmTues (list 'Sheen) 2))
    #f
   "not safe to add")
  
  (check-equal?
    (safe-add-stdnt-cw? (list TUE135)
                        (make-codewalk '1:35pmTues (list 'Sheen) 2))
    #t
   "safe to add"))