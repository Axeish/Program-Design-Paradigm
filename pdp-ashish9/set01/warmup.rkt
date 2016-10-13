;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(define TIME-ON-TASK 15)

;global constants

(define user1 "Ashish");string
(define user2 "Kumar");string
(define str "helloworld") ;string
(define img1 (rectangle 20 30 "solid" "green"));image
(define img2 (rectangle 40 10 "solid" "blue"));image
(define ball_img (circle 10 "solid" "black"));image
(define com_img (beside img1 img2 img1 img2 img1)) ;image


;exercise 13
;----------------------------------------------------------------------

;distFromOrigin: Number Number -> Number
;Will return the distance of point [x,y] from the origin
;Where:
;x is X-coordinate of the gven point
;y is Y-coordinate of the given point

;Example:
(begin-for-test
 (check-equal? (distFromOrigin 4 3 ) 5 "distance from origin"))

(define (distFromOrigin x y)
 (sqrt (+ (sqr x) (sqr y))))

;testing
(begin-for-test
  (check-equal? (distFromOrigin 3 4 ) 5 "distance from origin")
  (check-equal? (distFromOrigin -3 4 ) 5 "distance from origin")
  (check-equal? (distFromOrigin  -12 -5 ) 13 "distance from origin")
  (check-equal? (distFromOrigin 5 -12) 13 "distance from origin"))


;exercise 14
;--------------------------------------------------------------

 ;cube-volume: Number -> Number
;Will return the volume of the cube with given sides
;W here: 
;side is a non negative number
;cube is a non negative number
;Example :
(begin-for-test
  (check-equal? (cube-volume 3 ) 27 "Volume of a cube"))

(define (cube-volume side)
  ( * side side side))

;Testing:
(begin-for-test
  (check-equal? (cube-volume 2 ) 8 "Volume of a cube")
  (check-equal? (cube-volume 1.7 ) 4.913 "Volume of a cube"))


;exercise 15
;--------------------------------------------------------------

;string-first : string -> character
;Wil take a string as input and return first character as output
;Example:
(begin-for-test
  (check-equal? (string-first user1) "A" "first character"))

(define (string-first str)
  (string-ith str 0))

;Testing
(begin-for-test
  (check-equal? (string-first user1) "A" "first character")
  (check-equal? (string-first user2) "K" "first character"))

;exercise 16
;--------------------------------------------------------------

;string-last : string -> character
;Will take a string as input and return last character as output
;Example:
(begin-for-test
  (check-equal? (string-last user1) "h" "first character"))

(define (string-last str)
  (string-ith str (- (string-length str) 1)))

;Testing
(begin-for-test
  (check-equal? (string-last user1) "h" "last character")
  (check-equal? (string-last user2) "r" "last character"))

;exercise 17
;---------------------------------------------------------------------------------------------------------------------------------

;bool-imply : Boolean Boolean -> Boolean
;Will take b1 and b2 and return true if b1 is false or b2 is true 
;Where:
;b1 is a Boolean
;b2 is a Boolean
;Example :
(begin-for-test
  (check-true (bool-imply #true #true) "b2 is true"))

(define (bool-imply b1 b2)
  (or (false? b1) (not (false? b2))))

;Testing
(begin-for-test
  (check-true (bool-imply #true #true) "b2 is true")
  (check-false (bool-imply #true #false) "b1 is true , b2 is false")
  (check-true (bool-imply #false #true) "b1 is false"))


;exercise 18
;---------------------------------------------------------------------------------------------------------------------------------

;image-area : Image -> Number
;Will take an image and return number of pixels in that image
;Where:
;image-height is the height of image in pixels 
;image-width is width of image in pixels
;Example:
(begin-for-test
 (check-equal? (image-area img1)600 "number of pixels in image"))

(define (image-area img)
  (* (image-height img) (image-width img)))

;Testing
(begin-for-test
 (check-equal? (image-area img1) 600 "number of pixels in image")
 (check-equal? (image-area img2) 400 "number of pixels in image")
 (check-equal? (image-area com_img) 4200 "nuber of pixels in image"))
 
 
;exercise 19
;---------------------------------------------------------------------------------------------------------------------------------

;image-classify : Image-> String
;will take an image and return "tall", "wide" and "square" based on its dimension
;Where:
;image-height is the height of image in pixels 
;image-width is width of image in pixels
;Example:
(begin-for-test
 (check-equal? (image-classify img1) "tall" "image is tall"))
(define (image-classify img)
  (if (= (image-height img) (image-width img)) "square" (if (> (image-height img) (image-width img)) "tall" "wide" )))
(begin-for-test
   (check-equal? (image-classify img1) "tall" "image is tall")
   (check-equal? (image-classify img2) "wide" "image is wide")
   (check-equal? (image-classify (rectangle 20 20 "solid" "green")) "square" "image is square"))

;exercise 20
;---------------------------------------------------------------------------------------------------------------------------------

;string-join : String  String -> String
;Will take two string and join them with "_" in between.
;Where:
;str1 is a string that will be placed before "_"
;str2 is a string that will be placed after "_" 
;Example:
(begin-for-test
  (check-equal? (string-join user1 user2) "Ashish_Kumar" "string is joined"))

(define(string-join str1 str2) (string-append str1 "_" str2))

;Testing
(begin-for-test
  (check-equal? (string-join user1 user2) "Ashish_Kumar" "string is joined")
  (check-equal? (string-join user1 user1) "Ashish_Ashish" "string is joined"))


;exercise 21
;--------------------------------------------------------------------------------------------------------------------------------

;string-insert : String -> String
;Will take a string and insert "-" a given index
;Where:
;str is  a string given as input 
;index is integer between 0 and string-length of str.
;Example:
(begin-for-test
  (check-equal? (string-insert user1 0) "-Ashish" "string insert"))

(define (string-insert str index)
  (string-append (substring str 0 index) "-" (substring str index (string-length str)))
  )

;Testing
(begin-for-test
  (check-equal? (string-insert user2 2) "Ku-mar" "string insert")
  (check-equal? (string-insert user2 5) "Kumar-" "string insert"))


;exercise 22
;---------------------------------------------------------------------------------------------------------------------------------
;string-delete : String -> string 
;will delete a character at index position
;Where:
;str is  a string given as input 
;index is integer between 0 and 1 less than string-length of str.
;Example:
(begin-for-test
  (check-equal? (string-delete user1 0) "shish" "string deleted"))

(define (string-delete str index )
  (string-append (substring str 0 index) (substring str (+ index 1)  (string-length str) )))

(begin-for-test
  (check-equal? (string-delete user2 2) "Kuar" "string insert")
  (check-equal? (string-delete user2 4) "Kuma" "string insert"))
