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

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

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

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (weight branch)
    (if (number? branch-structure) branch-structure (total-weight branch-structure)))

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
  (map <??> m))
(define (transpose mat)
  (accumulate-n <??> <??> mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)))