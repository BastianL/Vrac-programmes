;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define a (make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE)))
     

(define b (make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE))

(define (contains-bt? BT n)
  (cond
    [(no-info? BT) #false]
    [(= n (node-ssn BT))#true]
    [else (or (contains-bt? (node-left BT) n) (contains-bt? (node-right BT) n))]))

(define (search-bt BT n)
  (cond
    [(no-info? BT) #false]
    [(= n (node-ssn BT))(node-name BT)]
    [else (value-or-data #false boolean? (search-bt (node-left BT) n) (search-bt (node-right BT) n))]))

(define (value-or-data value f0 f1 f2)
  (if (and (f0 f1) (f0 f2)) value (if (f0 f1) f2 f1)))

(define (in-order BT)
  (cond
    [(no-info? BT) '()]
    [else (append (in-order (node-left BT)) (list (node-ssn BT)) (in-order (node-right BT)))]))

(in-order a)

(define (search-bst BST n)
  (cond
    [(no-info? BST) NONE]
    [(< n (node-ssn BST)) NONE]
    [(= n (node-ssn BST)) (node-name BST)]
    [else (value-or-data NONE no-info? (search-bst (node-left BST)) (search-bst (node-right BST)))]))

(define (create-bst B N S)


  (define (substitute.v3 sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp)
               (if (equal? sexp old) new sexp)]
              [else
               (map for-sexp sexp)])))
    (for-sexp sexp)))