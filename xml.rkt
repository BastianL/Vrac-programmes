;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

'(transition ((from "seen-e")(to "seen-f")))

'(ul (li (word) (word))(li (word)))


(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; [List-of Attribute] or Xexpr.v2 -> ???
; determines whether x is an element of [List-of Attribute]
; #false otherwise
; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
;Xepr.v2 -> string
;extract the tag of the element representation
(define (xepr-name xe)
  (symbol->string(first xe)))

;Xepr.v2 -> [list-of-elements]
;retrive the elements content in xe

(define (xepr-content xe)
  (cond
    [(empty? xe) '()]
    [(symbol? (first xe)) (cons (symbol->string (first xe)) (xepr-content (rest xe)))]
    [(list? (first xe)) (if (list? (first (first xe))) (xepr-content (rest xe)) (cons (xepr-content (first xe)) (xepr-content (rest xe))))]))

(xepr-content '(ul (li (word) (word))(li (word))))
(xepr-content '(ul (li (word ((initial "a") (final "b"))) (word)) (li (word))))

(define (find-attr la symbol)
  (cond
    [(false? (assq symbol la)) #false]
    [else (second (assq symbol la))]))

; An XWord is '(word ((text String))).

(define Xword1 '(word ((text ""))))
(define Xword2 '(word ((text "ba"))))

(define (word? value)
  (and (list? value) (symbol? (first value)) (symbol=? (first value) 'word) (list? (second value)) (list? (first (second value)))(symbol? (first (first (second value)))) (symbol=? 'text (first (first (second value)))) (string? (second (first (second value))))))

(check-expect (word? Xword1) #true)

(define (word-text Xword)
  (second (first (second Xword))))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

; XItem.v1 -> Image 
; renders an item as a "word" prefixed by a bullet 
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

(define BT (circle 6 "solid" "black"))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

; XEnum.v1 -> Image 
; renders a simple enumeration as an image

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left
                          (render-item1 item)
                          so-far)))
    (foldr deal-with-one empty-image content)))

; An XItem.v2 is one of: 
; – (list 'li XWord )
; – (list 'li [List-of Attribute] (list XWord))
; – (list 'li XEnum.v2)
; – (list 'li [List-of Attribute] (list XEnum.v2))
; 
; An XEnum.v2 is one of:
; – (list 'ul [List-of XItem.v2])
; – (list 'ul [List-of Attribute] [List-of XItem.v2])

(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BT ; a graphical constant 
  (beside (circle 1 'solid BLACK) (text " " SIZE COLOR)))
 
; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BT item))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image 
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
      (cond
        [(word? content)
         (text (word-text content) SIZE BLACK)]
        [else (render-enum content)]))))

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two or three items items:
;   (cons FSM-State (cons FSM-State (cons keystrke '()))
; An FSM-State is a String that specifies a color
 
; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay (text current SIZE BLACK)(square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons (cons (cons initial (cons FSM-State '()))  [List-of X1T]))
; An X1T is a nested list of this shape:
;   (cons action (cons (cons state (cons FSM-State '())) (cons next (cons FSM-State '())))

(define BW (cons 'machine (cons `((initial ,"white")) (list `(action ((state ,"white") (next ,"black"))) `(action ((state ,"black") (next ,"white")))))))

; XMachine -> FSM-State
; simulates an FSM via the given configuration 
(define (simulate-xmachine xm)
  (simulate ... ...))
(check-expect (xm-state0 xm0) "red")
(check-expect (xm->transitions xm0) fsm-traffic)

; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))
 
; XMachine -> FSM-State 
; extracts and translates the transition table from xm0
 
(check-expect (xm-state0 xm0) "red")
 
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))
 
; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
 
(check-expect (xm->transitions xm0) fsm-traffic)
 
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; 
; An Attribute*.v3 is a [List-of Attribute.v3].
;   
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

(define PREFIX "Https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image 
          (define (render-stock-data w)
            (local (; [StockWorld String -> String] -> Image
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

(define (get-xepr Xepr s)
  (cond
    [(empty? Xepr) #false]
    