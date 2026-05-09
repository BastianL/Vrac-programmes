;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname backtrack) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

; A Node is a Symbol.

; A graph is a list of list
; represent nodes and their appropriates linked nodes.

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 
 
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
 
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)

(define (neighbors n g)
  (cond
    [(empty? g) '()]
    [(symbol=? (first (first g)) n) (second (first g))]
    [else (neighbors n (rest g))]))

(neighbors 'E sample-graph)

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [(boolean? candidate) #false]
    [else (cons origination (find-path/list (neighbors origination G) destination G))]))

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

(find-path 'A 'G sample-graph)

(define (test-on-all-nodes g)
  (local
    ((define (test-on-all-nodes-bis g1)
       (cond
         [(empty? g1) #true]
         [(test-on-one-node (first (first g1)) g) (test-on-all-nodes-bis (rest g1))]
         [else #false]))
     (define (test-on-one-node n g2)
       (cond
         [(empty? g2)#true]
         [(boolean? (find-path n (first (first g2)) g)) #false]
         [else (test-on-one-node n (rest g2))])))
    (test-on-all-nodes-bis g)))

(define cyclic-graph
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(define (fsm-match? an-fsm a-string)
  (local
    ;traverse the now list of string while keeping the full fsm in memory
  ((define (fsm-match2 cs ls)
     (local
       ((define transitions-liste (find-transitions cs (fsm-transitions an-fsm)))
        ;input false or the next state according to the proposed transittion
        (define (fsm-match3 lt)
          (cond
            [(empty? lt) #false]
            [(string=? (transition-key (first lt)) cs) (transition-next (first lt))]
            [else (fsm-match3 (rest lt))]))
        (define fsm-matched (fsm-match3 transitions-liste)))
        (cond
          [(empty? ls) cs]
          [(string? fsm-matched) (fsm-match2 fsm-matched (rest ls))]
          [else #false]))))
  (fsm-match2 (fsm-initial an-fsm) (explode a-string))))

;current state + liste of transition -> list of transition
;select all the transitions relevant to the state
(define (find-transitions state lt)
  (cond
    [(empty? lt) '()]
    [(string=? (transition-current (first lt)) state) (cons (first ls) (find-transitions state (rest lt)))]
    [else (find-transitions state (rest lt))]))

(define (fsm-match fsm string)
  (string=? (fsm-match? fsm string) (fsm-final fsm)))

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

(define (threatening? QP1 QP2)
  (local
    ((define sub-posn (make-posn (- (posn-x QP1) (posn-x QP2)) (- (posn-y QP1) (posn-y QP2))))
     (define diagonales (= (abs (posn-x sub-posn)) (abs (posn-y sub-posn)))))
  (not (or (= (posn-x QP1) (posn-x QP2)) (= (posn-y QP1) (posn-y QP2)) diagonales))))

(define Queen (circle 10 solid black)) 

(define (render-queens n lqp image)
  (cond
    [(empty? lqp)(empty-scene (* n (image-width image)) (* n (image-height image)))]
    [else (place-image (* (posn-x (first lqp)) (image-width image)) (* (posn-y (first lqp)) (image-height image)) image (render-queen n (rest Qlp) image))]))

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
 ; [List-of QP] or #false


(define (n-queens n)
  (local
    ((define (multi-threatening q qlp)
       (cond
         [(empty? qlp) #true]
         [(threatening? q (first qlp)) (multi-threatening q (rest qlp))]
         [else #false]))
     (define (pose-queen q qlp)
       (cond
         [(= (length qlp) n) qlp]
         [(= (posn-y q) n) '()]
         [(= (posn-x q) n) (pose-queen (make-posn 0 (add1 (posn-y q))) qlp)]
         [(multi-threatening q qlp) (pose-queen (make-posn 0 (add1 posn-y q)) (cons n qlp))]
         [else (pose-queen (make-posn (add1 (posn-x q)) (posn-y q)) qlp)]))
     (define (1st-queen q)
       (cond
         [(= (posn-y q) n) '()]
         [(= (posn-x q) n) (1st-queen (make-posn 0 (add1 (posn-y q))))]
         [else (cons (pose-queen q '()) (1st-queen (make-posn (add1 (posn-x q))(posn-y q))))])))
    (1st-queen (make-posn 0 0))))