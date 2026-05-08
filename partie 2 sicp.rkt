#lang sicp

(define (gcd i n)
  (define (gcalculd x)
    (cond
      ((< x 2) x)
      ((and (= (modulo i x) 0) (= (modulo n x) 0)) x)
          (else gcalculd (dec x))))
  (gcalculd i))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cons (/ n g) (/ g d))))
      
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-segment début fin)
  (cons début fin))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
              (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (rectangle1 segment1 segment2 segment3 segment4)
  (cons segment1 (cons segment2 (cons segment3 segment4))))

(define (rectangle2 point1 point2 point3 point4)
  (cons point1 (cons point2 (cons point3 point4))))

(define (perimeter rectangle)
  (*(+(get-size (get-longueur rectangle)) (get-size (get-largeur rectangle)))2))
(define (area rectangle)
  (*(get-size (get-longueur rectangle)) (get-size (get-largeur rectangle))))
(define (get-longueur rectangle)
  (make-segment (car rectangle) (cdr (car rectangle))))
(define (get-largeur rectangle)
  (make-segment (car (cdr (cdr rectangle))) (cdr (cdr (cdr rectangle)))))
(define (get-size segment)
  (+ (abs (- (x-point (start-segment segment)) (x-point (end-segment segment))))
     (abs (- (y-point (start-segment segment)) (y-point (end-segment segment))))))

(define (cons-alt x y)
  (lambda (m) (m x y)))

(define (car-alt z)
  (z (lambda (p q) p)))

(define (cdr-alt z)
  (z (lambda (p q) q)))

(define paire (cons-alt "premier" "deuxième"))
(define (cons-nombre x y)
  (* (expt 2 x) (expt 3 y)))

(define (car-nombre x)
  (if (= (modulo x 3) 0) (inc (car-nombre (/ 3 x))) -1 ))

(define (cdr-nombre x)
  (if (= (modulo x 2) 0) (inc (cdr-nombre (/ 2 x))) -1 ))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define (square x) (* x x))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))


(define (make-interval a b) (cons a b))
(define (carcdr paire)
  (lambda (f) (f (car paire) (cdr paire))))
(define (upper-bound interval) ((carcdr interval) max))
(define (lower-bound interval) ((carcdr interval) min))

(define (add-interval x y) 
  (make-interval (+ (lower-bound x) (lower-bound y)) 
                 (+ (upper-bound x) (upper-bound y)))) 

(define (sub-interval x y) 
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (= (upper-bound y) 0) (display "erreur division par zéro") (if (= (lower-bound y) 0) (display "erreur division par zéro")(mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))))

(define (mul-interval2 x y)
  (let ((lbx (lower-bound x))
        (lby (lower-bound y))
        (ubx (upper-bound x))
        (uby (upper-bound y)))
    (cond ((> lbx 0)
           (cond ((> lby 0) (make-interval (* lbx lby) (* ubx uby)))
                 ((< lby 0)
                  (cond ((> uby 0) (make-interval (* ubx lby) (* ubx uby)))
                        ((< uby 0) (make-interval (* ubx lby) (* lbx uby)))
                        ((= uby 0) (make-interval (* ubx lby) 0))))
                 ((= lby 0)
                  (if (> uby 0) (make-interval (0 (* ubx uby))) (make-interval (0 0))))))
          ((< lbx 0)
           (cond ((> ubx 0)
                   (cond ((> lby 0) (make-interval (* lbx uby) (* ubx uby)))
                        ((< lby 0)
                         (cond ((> uby 0) (mul-interval x y))
                               ((< uby 0) (make-interval (* ubx lby) (* lbx uby)))
                               ((= uby 0) (make-interval (* ubx lby) (* lbx lby)))))
                        ((= lby 0)
                         (if (> uby 0) (make-interval (* lbx uby) (* ubx uby)) (make-interval 0 0)))))
                 ((< ubx 0)
                  (cond ((> lby 0) (make-interval (* lbx uby) (* ubx lby)))
                        ((< lby 0)
                         (cond ((> uby 0) (make-interval (* lbx uby) (* lbx lby)))
                               ((< uby 0) (make-interval (* ubx uby) (* lbx lby)))
                               ((= uby 0) (make-interval (0 (* lbx lby))))))
                        ((= lby 0)
                         (if (> uby 0) (make-interval (* lbx uby) 0) (make-interval (0 0))))))
                 ((= ubx 0)
                  (cond ((< lby 0)
                         (if (> uby 0) (make-interval (* lbx uby) (* lbx lby)) (make-interval (0 (* lbx lby))))
                        ((> lby 0) (make-interval (* lbx uby) 0))
                        ((= lby 0) (if (> uby 0) (make-interval (* lbx uby) 0) (make-interval (0 0)))))))))
          ((= lbx 0)
           (if (> ubx 0)
               (cond ((< lby 0)
                      (if (> uby 0) (make-interval (* ubx lby) (* ubx uby)) (make-interval (* ubx lby) 0)))
                     ((> lby 0) (make-interval 0 (* ubx uby)))
                     ((= lby 0)
                      (if (> uby 0) (make-interval 0 (* ubx uby)) (make-interval 0 0))))
               (make-interval 0 0))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c perc)
  (make-center-width c (* c (/ perc 100))))

(define (percent i)
  (*(/ (center i) (width i)) 100))

(define (last-pair list)
  (if (null? (cdr list)) list (last-pair (cdr list))))

(define (reverse list)
  (define (add-element list)
    (if (null? (cdr list)) (car list) (cons (add-element (cdr list)) (car list))))
  (cons (add-element list) nil))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else
         (+ (cc amount
                (cdr coin-values))
            (cc (- amount
                   (car coin-values))
                coin-values)))))

(define (same-parity init . nombres)
  (define (check nombres f) (if (null? nombres) nil (if (f (car nombres)) (cons (car nombres) (check (cdr nombres) f)))))
  (if (even? init) (check nombres even?) (check nombres odd?)))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car (items))) (cdr (items)))))

(define (msquare-list items)
  (map square items))

(define (square-list-inv items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(define (for-each f list)
  (if (not (null? list)) (let () (f (car list)) (for-each f (cdr list)))))

(define (deep-reverse liste)
    (if (list? liste)
    (if (null? (cdr liste)) (car liste) (list (deep-reverse (cdr liste)) (deep-reverse (car liste))))
    liste))

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree)) (append (fringe (car tree))
                                    (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (list length structure))


(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;(define (weight branch)
    ;(if (number? branch-structure) branch-structure (total-weight branch-structure)))

(define (total-weight mobile)
  (+ (weight (left-branch mobile)) (weight (right-branch mobile))))

(define (balanced? mobile)
  (define (balanced-branch branch)
    (and (or (number? left-branch) (balanced? left-branch)) (or (number? left-branch) (balanced? left-branch)))
  )
  (and (balanced-branch (left-branch mobile)) (balanced-branch (right-branch mobile)) (= (* (branch-length left-branch) (weight left-branch)) (* (branch-length right-branch) (weight right-branch)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (tree-map operation tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree)
             (operation sub-tree)))
       tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (element) (cons (car s) element)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (length+ liste)
  (if (list? liste)
      (length liste)
      0))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef lower-terms) (+ lower-terms (* this-coef (expt x (length+ lower-terms)))))
              0
              coefficient-sequence))
(define (truecount-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (truecount-leaves (car x))
                 (truecount-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (n)
                     (if (list? n) (count-leaves n) 1))
                   t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (n) (dot-product n v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (define (prime-test x)
    (if (> x (sqrt n)) #t (if (= (modulo n x) 0) #f (prime-test (inc x)))))
  (prime-test 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triplets n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
       (map (lambda (k) (list i j k))
          (enumerate-interval 1 (- j 1))))
     (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triplets-sum t)
  (accumulate + 0 t))

(define (triplets-sums n s)
  (define (sum-desired? triple)
    (= s (accumulate + 0 triple)))
  (define (make-sum-of-triple triple)
    (append triple (list (accumulate + 0 triple))))
  (map make-sum-of-triple
       (filter sum-desired?
               (triplets n))))


(define empty-board
  nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (same-row pos1 pos2)
    (= (car pos1) (car pos2)))

(define (même-colonne pos1 pos2)
  (= (cdr pos1) (cdr pos2)))

(define (même-diagonale pos1 pos2)
  (let ((row1 (car pos1))
        (row2 (car pos2))
        (col1 (cadr pos1))
        (col2 (cadr pos2)))
    (= (abs (- row2 row1)) (abs (- col2 col1)))))

(define (get-queen-at-k k pos)
  (car (filter (lambda (q) (= (cadr q) k)) pos)))
(define (get-queens-not-at-k k pos)
  (filter (lambda (q) (not (= (cadr q) k))) pos))

(define (safe? k queens-in-pos)
  (let ((queen-at-k (get-queen-at-k k queens-in-pos))
        (queens-not-at-k (get-queens-not-at-k k queens-in-pos)))
    (if (equal? nil queens-not-at-k)
        #t
        (accumulate
         (lambda (a b) (and a b))
         #t
         (map (lambda (x) (not (or (same-row x queen-at-k)
                                   (même-diagonale x queen-at-k))))
              queens-not-at-k)))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (custom-equal liste1 liste2)
  (cond ((and (symbol? liste1) (symbol? liste2)) (eq? liste1 liste2))
        ((and (list? liste1) (list? liste2)) (and (custom-equal (car liste1) (car liste2)) (custom-equal (cdr liste1) (cdr liste2))))
        (else #f)))

(define (=number? n1 n2)
  (and (number? n1) (number? n2) (= n1 n2)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else (list base '** exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base x) (car x))
(define (exponent x) (caddr x))
(define (sum? x)
  (and (list? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend e)
  (if (not (null? (cadddr e)))
    (cddr e)
    (caddr e)))

(define (multiplicand e) 
  (if (not (null? (cadddr e)))
    (cddr e)
    (caddr e)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                     (deriv (base exp)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (list->tree (append (tree->list-1 set1) (tree->list-1 set2))))

(define (entry tree) (car tree))
(define (make-tree entry left right)
  (list entry left right))
;(define (left-branch tree) (cadr tree))
;(define (right-branch tree) (caddr tree))
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define binary-tree-1 (make-tree 7 (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil)) (make-tree 9 nil (make-tree 11 nil nil))))
(define binary-tree-2 (make-tree 3 (make-tree 1 nil nil) (make-tree 7 (make-tree 5 nil nil) (make-tree 9 nil (make-tree 11 nil nil)))))
(define binary-tree-3 (make-tree 5 (make-tree 3 (make-tree 1 nil nil) nil) (make-tree 9 (make-tree 7 nil nil) (make-tree 11 nil nil))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
(define (key number)
  number)

(define (lookup given-key set-of-records)
  (let ((current-item (key (entry set-of-records))))
  (cond ((null? set-of-records) false)
        ((equal? given-key current-item) (entry set-of-records))
        ((> given-key current-item) (lookup given-key (right-branch set-of-records)))
        ((< given-key current-item) (lookup given-key (left-branch set-of-records))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode-symbol symbol tree)
  (cond
    ((leaf? tree) (if (eq? symbol (symbol-leaf tree)) nil (error "mauvais symbol"))) 
    ((exist-within symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
    ((exist-within symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
    (else (error "symbol" symbol "non trouvé"))))
(define (exist-within symbol list)
  (cond
    ((null? list) #f)
    ((eq? (car list) symbol) #t)
    (else (exist-within symbol (cdr list)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))))

(define (put-in-place ordered-set element operation)
  (cond
    ((null? ordered-set) (list element))
    ((> (operation element) (operation (car ordered-set))) (append (car ordered-set) (put-in-place element (cdr ordered-set) operation)))
    (else (cons element ordered-set))))
(define chanson-pairs '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (YIP 9) (NA 16)))
(define chanson-tree (generate-huffman-tree chanson-pairs))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))