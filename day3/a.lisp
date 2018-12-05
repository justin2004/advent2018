;A claim like #123 @ 3,2: 5x4
; means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.

(defun rect (left right top bottom)
  (list left right top bottom))
(defun left (rect)
  (elt rect 0))
(defun right (rect)
  (elt rect 1))
(defun top (rect)
  (elt rect 2))
(defun bottom (rect)
  (elt rect 3))

(setf a (list (rect 1 5 -1 -10) (rect 3 5 -3 -6)))
(overlap? (car a) (cadr a))
(horizontal_overlap (car a) (cadr a))
(vertical_overlap (car a) (cadr a))
(left (car a))
(right (car a))
(top (car a))
(bottom (car a))
(cadr a)


;          -------------------                        rect1
; ------                                              rect2
;        -------
;                   -----
;     ----------------------------
;                           -----
;                                  -----

; if rect2.right >= rect1.left
;   & rect2.left <=rect1.right

(defun overlap? (rect1 rect2)
  (if (and 
        (and (>= (right rect2) (left rect1))
             (<= (left rect2) (right rect1)))
        (and (>= (top rect2) (bottom rect1))
             (<= (bottom rect2) (top rect1))))
      t
      nil))
(defun horizontal_overlap (rect1 rect2)
  (cond
    ((< (left rect2) (left rect1))  
     (- (right rect2) (left rect1)))
    ((> (right rect2) (right rect1))
     (- (right rect1) ) (left rect2))
    (t
     (min
       (abs (- (right rect2) (left rect2)))
       (abs (- (right rect1) (left rect1)))))))

(defun vertical_overlap (rect1 rect2) ; left -> bottom 
  (cond                               ; right -> top
    ((< (bottom rect2) (bottom rect1))  
     (- (top rect2) (bottom rect1)))
    ((> (top rect2) (top rect1))
     (- (top rect1) ) (bottom rect2))
    (t
     (min
       (abs (- (top rect2) (bottom rect2)))
       (abs (- (top rect1) (bottom rect1)))))))

(right (rect :left 44))

in (x,y)
ul (3,2)
ur (8,2)
ll (3,6)
lr (8,6)


"#1 @ 1,3: 4x4"
"#2 @ 3,1: 4x4"
"#3 @ 5,5: 2x2"

"How many square inches of fabric are within two or more claims?"

