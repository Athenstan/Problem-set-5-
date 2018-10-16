# Problem-set-5-

(require 2htdp/image)
(require spd/tags)
;; DO NOT PUT ANY PERSONALLY IDENTIFYING INFORMATION IN THIS FILE. 
;; YOUR COMPUTER SCIENCE IDs WILL BE SUFFICIENT TO IDENTIFY YOU 
;; AND, IF YOU HAVE ONE, YOUR PARTNER

; Computer Science 110
; 2018W1
; 
; Graded Problem Set 5
; 
; Computer Science id (Student 1): ________
; Computer Science id (Student 2): ________




; Problem 1
; Before Netflix there was Blockbuster. Let's imagine a world in which
; Blockbuster still exists and you have been tasked with developing
; a program that produces a list of movie titles, in a specific
; order based on certain criteria. 
; 
; Design a function that consumes a
;    - a minimum rating
;    - a genre
;    - a ListOfMovie
; 
; and produces the names of movies that have a rating greater or
; equal to the minumum, and that match the genre.  The names
; must be sorted in alphabetical order.
; 
; Your function must be called select-movies.  It must be designed
; as a composition of three other functions called movie-titles,
; sort-movies, and filter-movies - in that order in the body
; of select-movies.
; 
; Be sure to follow all applicable helper and other design rules.
; 
; You must use these data definitions, with these names.
; 



;; === Data Definitions ===
(@HtDD Movie)
(define-struct movie (title revenue genre rating))
;; Movie is (make-movie String Natural String Number)
;; interp. information about a movie,
;;         title is the name of the movie 
;;         revenue (in millions of dollars) is the amount
;;         earned at the box office, genre is its genre
;;         rating is the critics rating,
;;         should always be between 1 and 5.


(define M1 (make-movie "Titanic" 659 "Romance" 3.9))
(define M2 (make-movie "Black Panther" 700 "Action" 3.7))
(define M3 (make-movie "Tomb Raider"  57 "Action" 3.2))
(define M4 (make-movie "Sound of Music" 163 "Musical" 4))
(define M5 (make-movie "Avengers Infinity War" 678 "Action" 4.3))
(define M6 (make-movie "The Predator" 48 "Horror" 3))
(define M7 (make-movie "Slender Man" 30 "Horror" 1.5))
(define M8 (make-movie "Crazy Rich Asians" 166 "Romance" 3.8))
(define M9 (make-movie "Mamma Mia" 120 "Musical" 3.6))
(define M10 (make-movie "The First Purge" 69 "Horror" 2.6))


(@dd-template-rules compound)

(define (fn-for-movie m)
  (... (movie-title m)
       (movie-revenue m)
       (movie-genre m)
       (movie-rating m)))

(@HtDD ListOfMovie)
;; ListOfMovie is one of:
;; - empty
;; - (cons Movie ListOfMovie)
;; interp. a list of movies

(define LOM1 (cons M1 empty))
(define LOM2 (cons M2 LOM1))
(define LOM3 (cons M3 LOM2))
(define LOM4 (cons M4 LOM3))
(define LOM5 (cons M5 LOM4))
(define LOM6 (cons M6 LOM5))
(define LOM7 (cons M7 LOM6))
(define LOM8 (cons M8 LOM7))
(define LOM9 (cons M9 LOM8))
(define LOM10 (cons M10 LOM9))


(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    ref
                    self-ref)

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-movie (first lom))
              (fn-for-lom (rest lom)))]))

(@HtDD ListOfString)
;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LOS0 empty)
(define LOS1 (cons "bat" LOS0))
(define LOS2 (cons "cat" LOS1))

(@dd-template-rules one-of
                    atomic-distinct
                    compound
                    self-ref)

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))


;(define M1 (make-movie "Titanic" 659 "Romance" 3.9))
;(define M2 (make-movie "Black Panther" 700 "Action" 3.7))
;(define M3 (make-movie "Tomb Raider"  57 "Action" 3.2))
;(define M4 (make-movie "Sound of Music" 163 "Musical" 4))
;(define M5 (make-movie "Avengers Infinity War" 678 "Action" 4.3))
;(define M6 (make-movie "The Predator" 48 "Horror" 3))
;(define M7 (make-movie "Slender Man" 30 "Horror" 1.5))
;(define M8 (make-movie "Crazy Rich Asians" 166 "Romance" 3.8))
;(define M9 (make-movie "Mamma Mia" 120 "Musical" 3.6))
;(define M10 (make-movie "The First Purge" 69 "Horror" 2.6))


;; === Functions ====
(@Problem 1)
;(@HtDF select-movies)
(@signature ListOfMovie Number String -> ListOfMovie)
;; selects movies with the same genre and equal or Higher rating.
(check-expect (select-movies (cons M1 (cons M2  (cons M3 empty))) 3.5 "Action")
              (cons M1 (cons M2 empty)))
(check-expect (select-movies (list M5 M6 M7 M8 M9 M10 empty) 1 "Horror")
              (list M6 M7 M10 empty)) 

     
                             
              
                             
                             
;!!! uncomment this line when you start your problem set

(define (select-movie lom) lom)

