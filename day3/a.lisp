(ql:quickload "alexandria")
(ql:quickload "regex")
(ql:system-apropos "regex")
;A claim like #123 @ 3,2: 5x4
; means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.

(read_file "/home/justin/aoc_2018/day3/input/data.prep")
(read_file "/home/justin/aoc_2018/day3/input/smalldata.prep")
(setf tab (make-hash-table :test 'equal))
tab
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) tab)

(let ((total 0))
  (maphash #'(lambda (k v) (if (not (= 1 v)) (incf total))) tab)
  total)

(mapcar #'(lambda (rect) (clah rect tab)) (read_file "/home/justin/aoc_2018/day3/input/data.prep"))

(defun read_file (filename)
  (with-open-file (f filename :direction :input)
  (defun getlines(current_seq_of_lines)
        (let ((line (read f nil 'done nil)))
          (if (eq line 'done)
              current_seq_of_lines
              (getlines (append current_seq_of_lines (list line))))))
  (getlines '())))

;(regex:scan-str (regex:compile-str "are\([a-z]*\)") "hello theare sir. this is a house.")

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

(alexandria:iota 5 :start 1)

(setf tab (make-hash-table :test 'equal))
(setf (gethash '(1 3) a) 1)
(setf (gethash '(3 3) a) 2)
(gethash '(3 3) a)
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) tab)

(defun rect_to_horizontal_strips (rect ht)
  (let ((rows (- (top rect) (bottom rect)))
        (cols (- (right rect) (left rect))))
    (mapcar #'(lambda (x_offset y_offset) 
                (mapcar #'(lambda (x_offset) (setf (gethash (list (+ x_offset (left rect)) (+ y_offset (top rect))) ht)
                                               (+ (gethash (list (+ x_offset (left rect)) (+ y_offset (top rect))) ht) 1))))))))
                                               ; maybe use let so we dont have to recalc

    

(defun blah (rect)
  (mapcar #'(lambda (y)
              (mapcar #'(lambda (x) (list x y))
                      (alexandria:iota (- (right rect) (left rect)) :start (left rect))))
          (alexandria:iota (- (top rect) (bottom rect)) :start (bottom rect))))

(mapcar #'(lambda (ordered_pair) (incf_cbn (gethash ordered_pair tab))) (flatten (blah (car a))))
tab

(clah (car a) tab)
tab

(setf u nil)
(null u)
(incf_cbn u)
(defmacro incf_cbn (place)  ; a function would not work here. i needed incf_cbn to not eval place right away
  "incf could be null"
  `(if (null ,place)
       (setf ,place 1)
       (incf ,place)))

(defun clah (rect ht)
  (mapcar #'(lambda (ordered_pair) (incf_cbn (gethash ordered_pair ht)))    (flatten (blah rect))))

(defun flatten (lis)
  (if (and (listp lis)
           (not (null lis)))
      (if (listp (car lis))
          (append (car lis)         (flatten (cdr lis)))
          (append (list (car lis))  (flatten (cdr lis))))
      lis))
