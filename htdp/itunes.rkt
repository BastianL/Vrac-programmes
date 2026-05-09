;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname itunes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; the 2htdp/itunes library documentation, part 1: 
(require 2htdp/itunes)


; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
; LLists


(define date1(create-date 1997 08 06 20 14 03))
(define date2(create-date 2014 11 07 14 32 54))
(define date3(create-date 1256 04 23 15 45 23))
(define date4(create-date 0521 05 30 08 15 45))
(define track1(create-track "début" "bastion" "commencement" 24 1 date1 2 date2))
(define track2(create-track "seconde" "jean" "ancien" 12 2 date3 5 date4))
(define track3(create-track "bla" "blah" "commencement" 01 2 date1 3 date2))
(define LTrack1 (list track1 track2 track3))

(define (total-time lt)
  (cond
    [(empty? lt) 0]
    [(track? (first lt)) (+ (track-time (first lt))(total-time (rest lt)))]))

(define (select-all-album-titles lt)
  (cond
    [(empty? lt) '()]
    [(track? (first lt)) (cons (track-album (first lt)) (select-all-album-titles (rest lt)))]))

(define (create-set ls)
  (cond
    [(empty? ls) '()]
    [(check-list (rest ls) (first ls))(cons (first ls)(create-set(rest ls)))]
    [else (create-set (rest ls))]))

(check-expect (check-list (list "a" "b" "c" "d") "a") #false)
(check-expect (check-list (list "a" "b" "c" "d") "c") #false)
(define (check-list ls s)
  (cond
    [(empty? ls) #true]
    [(string? (first ls)) (if (string=? (first ls) s) #false (check-list (rest ls) s))]
    [else (check-list (rest ls) s)]))

(define (select-album-titles/unique lt)
  (create-set (select-all-album-titles lt)))
;prend un nom d'album et une liste de piste, sort une liste de piste contenant uniquement celle avec l'album entré en paramètre
(define (select-album a lt)
  (cond
    [(empty? lt) '()]
    [(string=? (track-album (first lt)) a)(cons (first lt)(select-album a (rest lt)))]
    [else (select-album a (rest lt))]))

(define (select-album-date a d lt)
  (select-date d (select-album a lt)))

(define (select-date d lt)
  (cond
    [(empty? lt) '()]
    [(date-check (track-played (first lt)) d)(cons (first lt)(select-date d (rest lt)))]
    [else (select-date d (rest lt))]))
;prend une liste de piste, sort une liste de piste sans doublons dans les albums
(define (select-albums lt)
  (cond
    [(empty? lt) '()]
    [(check-list (select-all-album-titles (rest lt)) (track-album (first lt)))(cons (first lt) (select-albums (rest lt)))]
    [else (select-albums (rest lt))]))

; the 2htdp/itunes library documentation, part 2: 
 
; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
;

(define LAssoc1 (list (list "name" "Chantons") (list "artist" "Julien") (list "album" "L'or") (list "time" 10897) (list "track#" 2) (list "added" (create-date 1933 12 5 12 2 32)) (list "play#" 5) (list "played" (create-date 2015 5 9 13 25 12))))
(define LAssoc2 (list (list "played" #true) (list "name" "Chanson") (list "artist" "Chanteur")))
(define LLists (list LAssoc1 LAssoc2))
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; LLists
(check-expect (find-association "artist" LAssoc1 0) "Julien")
(define (find-association key La default)
  (cond
    [(empty? La) default]
    [(string? La)(if (string=? (rest (first La)) key)(first (rest (first La)))(find-association key (rest La) default))]
    [else (find-association key (rest La) default)]))

(define (total-time/list Lli)
  (cond
    [(empty? Lli) 0]
    [else (+(find-association "Total Time" (first Lli) 0)(total-time/list (rest Lli)))]))
; prend une llist, sort une liste de string comprenant tout les attributs boolean

(define (boolean-attributes llst)
  (create-set
   (cond [(empty? llst) '()]
         [else (append (find-booleans (first llst))
                       (boolean-attributes (rest llst)))])))

; LAssoc -> List-of-string
; produce a list of keys (String) for any attribute in a LTrack is a boolean
(check-expect (find-booleans '()) '())
(check-expect (find-booleans (list (list "aa" "bb"))) '())
(check-expect (find-booleans (list (list "aa" "bb")
                                   (list "cc" #true)
                                   (list "dd" 3)
                                   (list "ee" #false)))
              (list "cc" "ee"))

(define (find-booleans las)
  (cond [(empty? las) '()]
        [else (if (boolean? (second (first las)))
                  (cons (first (first las))
                        (find-booleans (rest las)))
                  (find-booleans (rest las)))]))
;check if d1 is superior to d2
(define (date-check d1 d2)
  (cond
    [(>(date-year d1)(date-year d2))#true]
    [(<(date-year d1)(date-year d2))#false]
    [(=(date-year d1)(date-year d2))(date-check2 d1 d2)]))

(define (date-check2 d1 d2)
  (cond
    [(>(date-month d1)(date-month d2))#true]
    [(<(date-month d1)(date-month d2))#false]
    [(=(date-month d1)(date-month d2))(date-check3 d1 d2)]))

(define (date-check3 d1 d2)
  (cond
    [(>(date-day d1)(date-day d2))#true]
    [(<(date-day d1)(date-day d2))#false]
    [(=(date-day d1)(date-day d2))(date-check4 d1 d2)]))

(define (date-check4 d1 d2)
  (cond
    [(>(date-hour d1)(date-hour d2))#true]
    [(<(date-hour d1)(date-hour d2))#false]
    [(=(date-hour d1)(date-hour d2))(date-check5 d1 d2)]))

(define (date-check5 d1 d2)
  (cond
    [(>(date-minute d1)(date-minute d2))#true]
    [(<(date-minute d1)(date-minute d2))#false]
    [(=(date-minute d1)(date-minute d2))(date-check6 d1 d2)]))

(define (date-check6 d1 d2)
  (cond
    [(>(date-second d1)(date-second d2))#true]
    [(<(date-second d1)(date-second d2))#false]
    [(=(date-second d1)(date-second d2))#false]))
