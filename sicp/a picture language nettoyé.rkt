#lang sicp
(#%require sicp-pict)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define zorro mark-of-zorro)

(define (split op1 op2)
   (define (split-recursive painter n)
       (if (= n 0)
           painter
           (let ((smaller (split-recursive painter (- n 1))))
             (op1 painter (op2 smaller smaller)))))
  (lambda (painter n) (split-recursive painter n)))

(define right-split (split beside below))

(define up-split (split below beside))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split2 painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-vert (lambda (l) (flip-horiz (flip-vert l)))
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))


(define (xcor-vect vect)
  (vector-xcor vect))

(define (ycor-vect vect)
  (vector-ycor vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (ycor-vect vect2)) (+ (xcor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
    (make-vect (- (xcor-vect vect1) (ycor-vect vect2)) (- (xcor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalar vect)
  (make-vect (* (xcor-vect vect) scalar) (* (ycor-vect vect) scalar)))


(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mv x y) (make-vect x y))

(define (outline frame)
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 0) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1)))))

(define (xpicture frame)
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                          (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define (diamond frame)
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (nothing23 painter)
  (transform-painter painter (mv 0 0) (mv 0 0) (mv 0 0)))

(define (nothing24 painter)
  (transform-painter painter (mv 0 0) (mv 0 0) (mv 0 1)))

(define (nothing25 painter)
  (transform-painter painter (mv 0 0) (mv 0 0) (mv 1 0)))

(define (nothing26 painter)
  (transform-painter painter (mv 0 0) (mv 0 0) (mv 1 1)))

(define (nothing27 painter)
  (transform-painter painter (mv 0 0) (mv 0 1) (mv 0 0)))

(define (nothing28 painter)
  (transform-painter painter (mv 0 0) (mv 0 1) (mv 0 1)))

(define (rotate+flip painter)
  (transform-painter painter (mv 0 0) (mv 0 1) (mv 1 0)))

(define (rotate+flip+triangleur painter)
  (transform-painter painter (mv 0 0) (mv 0 1) (mv 1 1)))

(define (nothing29 painter)
  (transform-painter painter (mv 0 0) (mv 1 0) (mv 0 0)))

(define (identique painter)
  (transform-painter painter (mv 0 0) (mv 1 0) (mv 0 1)))

(define (nothing30 painter)
  (transform-painter painter (mv 0 0) (mv 1 0) (mv 1 0)))

(define (trianglebr painter)
  (transform-painter painter (mv 0 0) (mv 1 0) (mv 1 1)))

(define (nothing31 painter)
  (transform-painter painter (mv 0 0) (mv 1 1) (mv 0 0)))

(define (triangleur painter)
  (transform-painter painter (mv 0 0) (mv 1 1) (mv 0 1)))

(define (rotate+flip+trianglebr painter)
  (transform-painter painter (mv 0 0) (mv 1 1) (mv 1 0)))

(define (nothing32 painter)
  (transform-painter painter (mv 0 0) (mv 1 1) (mv 1 1)))

(define (nothing33 painter)
  (transform-painter painter (mv 0 1) (mv 0 0) (mv 0 0)))

(define (nothing34 painter)
  (transform-painter painter (mv 0 1) (mv 0 0) (mv 0 1)))

(define (rotate270+trianglebl painter)
  (transform-painter painter (mv 0 1) (mv 0 0) (mv 1 0)))

(define (rotate270 painter)
  (transform-painter painter (mv 0 1) (mv 0 0) (mv 1 1)))

(define (nothing35 painter)
  (transform-painter painter (mv 0 1) (mv 0 1) (mv 0 0)))

(define (nothing36 painter)
  (transform-painter painter (mv 0 1) (mv 0 1) (mv 0 1)))

(define (nothing37 painter)
  (transform-painter painter (mv 0 1) (mv 0 1) (mv 1 0)))

(define (nothing38 painter)
  (transform-painter painter (mv 0 1) (mv 0 1) (mv 1 1)))

(define (rotate+flipvert+trianglebl painter)
  (transform-painter painter (mv 0 1) (mv 1 0) (mv 0 0)))

(define (flipvert painter)
  (transform-painter painter (mv 0 1) (mv 1 1) (mv 0 0)))

(define (nothing painter)
  (transform-painter painter (mv 0 1) (mv 1 1) (mv 0 1)))

(define (flipvert+triangleur painter)
  (transform-painter painter (mv 0 1) (mv 1 1) (mv 1 0)))

(define (nothing2 painter)
  (transform-painter painter (mv 0 1) (mv 1 1) (mv 1 1)))

(define (nothing3 painter)
  (transform-painter painter (mv 1 0) (mv 0 0) (mv 0 0)))

(define (flip+trianglebl painter)
  (transform-painter painter (mv 1 0) (mv 0 0) (mv 0 1)))

(define (nothing4 painter)
  (transform-painter painter (mv 1 0) (mv 0 0) (mv 1 0)))

(define (flip-horiz painter)
  (transform-painter painter (mv 1 0) (mv 0 0) (mv 1 1)))

(define (rotate+trianglebr painter)
  (transform-painter painter (mv 1 0) (mv 0 1) (mv 0 0)))

(define (nothing5 painter)
  (transform-painter painter (mv 1 0) (mv 0 1) (mv 0 1)))

(define (nothing6 painter)
  (transform-painter painter (mv 1 0) (mv 0 1) (mv 1 0)))

(define (flip+triangleur painter)
  (transform-painter painter (mv 1 0) (mv 0 1) (mv 1 1)))

(define (nothing7 painter)
  (transform-painter painter (mv 1 0) (mv 1 0) (mv 0 0)))

(define (nothing8 painter)
  (transform-painter painter (mv 1 0) (mv 1 0) (mv 0 1)))

(define (nothing9 painter)
  (transform-painter painter (mv 1 0) (mv 1 0) (mv 1 0)))

(define (nothing10 painter)
  (transform-painter painter (mv 1 0) (mv 1 0) (mv 1 1)))

(define (rotate90 painter)
  (transform-painter painter (mv 1 0) (mv 1 1) (mv 0 0)))

(define (rotate90+triangleur painter)
  (transform-painter painter (mv 1 0) (mv 1 1) (mv 0 1)))

(define (nothing11 painter)
  (transform-painter painter (mv 1 0) (mv 1 1) (mv 1 0)))

(define (nothing12 painter)
  (transform-painter painter (mv 1 0) (mv 1 1) (mv 1 1)))

(define (nothing13 painter)
  (transform-painter painter (mv 1 1) (mv 0 0) (mv 0 0)))

(define (rotate+flipvert+triangleul painter)
  (transform-painter painter (mv 1 1) (mv 0 0) (mv 0 1)))

(define (rotate270+trianglebr painter)
  (transform-painter painter (mv 1 1) (mv 0 0) (mv 1 0)))

(define (nothing14 painter)
  (transform-painter painter (mv 1 1) (mv 0 0) (mv 1 1)))

(define (rotate180+triangleul painter)
  (transform-painter painter (mv 1 1) (mv 0 1) (mv 0 0)))

(define (nothing15 painter)
  (transform-painter painter (mv 1 1) (mv 0 1) (mv 0 1)))

(define (rotate180 painter)
  (transform-painter painter (mv 1 1) (mv 0 1) (mv 1 0)))

(define (nothing16 painter)
  (transform-painter painter (mv 1 1) (mv 0 1) (mv 1 1)))

(define (rotate90+flipvert+trianglebr painter)
  (transform-painter painter (mv 1 1) (mv 1 0) (mv 0 0)))

(define (rotate90+flipvert painter)
  (transform-painter painter (mv 1 1) (mv 1 0) (mv 0 1)))

(define (nothing17 painter)
  (transform-painter painter (mv 1 1) (mv 1 0) (mv 1 0)))

(define (nothing18 painter)
  (transform-painter painter (mv 1 1) (mv 1 0) (mv 1 1)))

(define (nothing19 painter)
  (transform-painter painter (mv 1 1) (mv 1 1) (mv 0 0)))

(define (nothing20 painter)
  (transform-painter painter (mv 1 1) (mv 1 1) (mv 0 1)))

(define (nothing21 painter)
  (transform-painter painter (mv 1 1) (mv 1 1) (mv 1 0)))

(define (nothing22 painter)
  (transform-painter painter (mv 1 1) (mv 1 1) (mv 1 1)))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(paint (square-limit einstein 1))
(paint (rotate180 (square-limit einstein 1)))
(paint (square-limit2 einstein 1))
