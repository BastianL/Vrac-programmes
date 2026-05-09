;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dictionary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define LOCATION "linuxwords.txt")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

;consume a letter and a dictionarry, give a number
;say how many words in the dictionary begin with this letter
(define (start-with# let dic)
  (cond
    [(empty? dic)0]
    [(string=? let (string-ith (first dic) 0))(+ (start-with# let (rest dic)) 1)]
    [else (start-with# let (rest dic))]))

(define-struct Letter-Counts [letter count])

;consomme un dictionnaire, donne une liste de letter-count
;donne le nombre de mot commençant par chque lettre de l'alphabet

(define (count-by-letter dic)
  (design-choice dic LETTERS))

;consomme les lettres assignées, donne la liste des letters-count
(define (design-choice dic lets)
  (cond
    [(empty? lets) '()]
    [else (cons (make-Letter-Counts (first lets) (start-with# (first lets) dic)) (design-choice dic (rest lets)))]))

;consomme un dictionnaire, donne un letter-count
(define (most-frequent dic)
  (first (dic-list-sorted dic LETTERS 0)))

(define (dic-list-sorted dic lets mem)
  (cond
    [(empty? lets) '()]
    [(> (start-with# (first lets) dic) mem)(cons (make-Letter-Counts (first lets) (start-with# (first lets) dic)) (dic-list-sorted dic (rest lets) (start-with# (first lets) dic)))]
    [else (dic-list-sorted dic (rest lets) mem)]))

;consomme un dictionnaire et donne une liste de dictionnaire, un par lettre
(define (words-by-first-letter dic)
  (dic-letters dic LETTERS))

(define (dic-letters dic l)
  (cond
    [(empty? l) '()]
    [(string? (first l)) (cons (start-with dic (first l))(dic-letters dic (rest l)))]))

(define (start-with dic l)
  (cond
    [(empty? dic) '()]
    [(string=? (string-ith (first dic) 0) l) (cons (first dic) (start-with (rest dic) l))]
    [else (start-with (rest dic) l)]))

(check-expect (most-frequent AS-LIST)
              (most-frequent.v2 AS-LIST))

(define (most-frequent.v2 dic)
  (first (dic-list-sorted.v2 (words-by-first-letter dic) 0)))

(define (dic-list-sorted.v2 dic² mem)
  (cond
    [(empty? dic²) '()]
    [(cons? (first dic²))(if (>(count-list (first dic²))mem) (cons (string-ith (first (first dic²)) 0) (dic-list-sorted.v2 (rest dic²)(count-list (first dic²))))(dic-list-sorted.v2 (rest dic²) mem))]
    [(empty? (first dic²)) (dic-list-sorted.v2 (rest dic²) mem)]))

(define (count-list l)
  (cond
    [(empty? l) 0]
    [else (+ (count-list (rest l))1)]))