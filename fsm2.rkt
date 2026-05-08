;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fsm2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
;   - (cons ktransition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define fsm-109
  (list (make-ktransition "start" "a" "expect")
        (make-ktransition "expect" "b" "finished")
        (make-ktransition "expect" "c" "finished")
        (make-ktransition "expect" "d" "finished")
        (make-ktransition "finished" "d" "start")))

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))
;prend deux états et dit si les deux etats sont egaux

(define bw-machine
  (list (make-transition "white" "black")
        (make-transition "black" "white")))
(define (state=? state1 state2)
  (string=? state1 state2))

; FSM -> ???
; match the keys pressed with the given FSM 
(define (simulate an-fsm)
  (big-bang ...
    [to-draw ...]
    [on-key ...]))


; SimulationState.v1 -> Image
; renders a world state as an image 
(define (render-state.v1 s)
  empty-image)
 
; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
   cs)

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image 
(define (render-state.v2 s)
  empty-image)

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-string]
    [on-key find-next-state.v2]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

;SimulationState.v2 -> String
; renders current world state as a string

(define (state-as-string an-fsm)
  (text (fs-current an-fsm) 24 "black"))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from an-fsm and ke
(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm))))

(define (find-next-state.v2 an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find.v2 (fs-fsm an-fsm) ke (fs-current an-fsm))))

(define (find.v2 transitions ke current)
  (cond
    [(empty? transitions)(string-append "not found: " current " for : " ke)]
    [(state=? (ktransition-current (first transitions)) current)(if (string=? ke (ktransition-key (first transitions)))(ktransition-next (first transitions))(find.v2 (rest transitions) ke current))]
    [else (find.v2 (rest transitions) ke current)]))

(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))

(check-expect
  (find-next-state (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))
(check-expect
  (find-next-state (make-fs fsm-traffic "green") "b")
  (make-fs fsm-traffic "yellow"))

(check-expect
  (find-next-state (make-fs fsm-traffic "yellow") "q")
  (make-fs fsm-traffic "red"))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black")
             "not found: black")
(define (find transitions current)
  (cond
    [(empty? transitions)(string-append "not found: " current)]
    [(state=? (transition-current (first transitions)) current)(transition-next (first transitions))]
    [else (find (rest transitions) current)]))