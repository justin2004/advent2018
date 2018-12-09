; TODO next day i must grow the functions from the bottom and don't let different representations stick around 

(defmethod print-object ((p mypoint) stream)
  (format stream "<x:~A y:~A>" (x p) (y p)))

; for each prime point count each in the ht
(mapcar #'(lambda (prime_point)
            (let ((total 0))
            (maphash #'(lambda (k v) 
                         (if (not (null v))
                           (if (mypoint_equal? (cadar v) prime_point)
                               (incf total))))
                     ht)
            (list total prime_point)))
        (list a b))



(find 55 (list 1 3 7 5 4))
(remove 4 (list 1 2 3 4 5 6 4 ))
(cancel_ties (list '(1 828282) '(2 8334) '(3 38438484) '(2 838383)))
(cancel_ties (list '(1 828282) '(2 8334) '(3 38438484) '(6 838383)))

(maphash #'(lambda (k v)
             (let ((current_val (gethash k ht)))
               (setf (gethash k ht) (cancel_ties current_val))))
         ht)

(defun cancel_ties (lis)
  "deletes values with tied distances or returns lists untouched "
  (let ((lis_cp lis))
  (do ((i 0 (+ 1 i)))
      ((= i (length lis)) 'done)
       (if (> (count (car (elt lis i)) lis :key #'car) 1)
           (setf lis_cp nil)))
  lis_cp))
           


  
  
(histo (list '(1 828282) '(2 8334) '(3 38438484) '(2 838383)))
(remove_equals (list '(1 828282) '(2 8334) '(3 38438484) '(2 838383)))
(defun remove_equals (lis)
  (count 2 lis :key #'(lambda (ele) (car ele))))


(defclass mypoint ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))
(defmethod mypoint_equal? ((a mypoint) (b mypoint))
  (and
    (= (x a) (x b))
    (= (y a) (y b))))
    
(setf a (make-instance 'mypoint))
(setf b (make-instance 'mypoint))
(make-instance 'mypoint :x 4)
(setf (x a) 3)
(setf (y a) -1)
(setf (x b) 2)
(setf (y b) -4)
(x a)
(describe a)
; . . a . .
; . . . . .
; . . . . .
; . b . . . 
; . . . . .
(manhattan_distance a b)
(max_lis '(4 5 6))
(max (list 4 5 6))


(defun max_lis (lis)
  (car (last (sort lis #'<))))

(defun min_lis (lis)
  (car (last (sort lis #'>))))

(max_lis (list 43 58 2))
(describe (avg_points (list a b)))
(manhattan_distance b (avg_points (list a b)))
(describe (avg_points (list a b)))
(/ 5 2.0)
(defun avg_points (lis) ; lis of points
  (let ((xes (mapcar #'(lambda (point) (x point)) lis))
        (yes (mapcar #'(lambda (point) (y point)) lis)))
    (make-instance 'mypoint
                   :x
                   (/ (reduce #'+ xes) (length xes))
                   :y
                   (/ (reduce #'+ yes) (length yes)))))

(setf ht (make-hash-table :test #'equal))
ht
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) ht)
(mapcar #'(lambda (op) (setf (gethash op ht) nil)) (bounding_rect_opairs (list a b)))
;ht is loaded

(beta ht a)

; for each of the regular points, loop thru the prime points, get the mdist btween them and sort and get the prime point with the lowest mdist
(maphash #'(lambda (regular_op closest_prime_op)
                     (setf (gethash regular_op ht)
                     (sort 
                       (mapcar #'(lambda (prime_pt)
                                   (list
                                     (manhattan_distance prime_pt (make-instance 'mypoint :x (car regular_op) :y (cadr regular_op)))
                                     prime_pt))
                               (list a b)) #'< :key #'car)))
         ht)



(defun beta (ht point) ; ht contains   k: ordered pair   v: nearest prime ordered pair        point: one of the prime points
  (maphash #'(lambda (k v) 
               (format t "dist between point and this op ~A in ht ~A~%" 
                       k 
                       (manhattan_distance (make-instance 'mypoint :x (car k) :y (cadr k)) point)))
           ht))

(mapcar #'(lambda (op)
            (manhattan_distance
              (make-instance 'mypoint :x (car op) :y (cadr op))
              a))
        (bounding_rect_opairs (list a b)))

(defun bounding_rect_opairs (lis) ; lis of points
"returns all the order pairs in the bounding rectangle for the lis of points "
  (flatten
    (rect_to_ordered_pairs
      (rect (car (min_x_y lis))
            (+ (car  (max_x_y lis)) 1)
            (+ (cadr (max_x_y lis)) 1)
            (cadr    (min_x_y lis))
            nil)))) ; not using id

           
(max_x_y (list a b))
(min_x_y (list a b))

(defun max_x_y (lis) ; lis of points
  (let ((xes (mapcar #'(lambda (point) (x point)) lis))
        (yes (mapcar #'(lambda (point) (y point)) lis)))
    (list
      (max_lis xes)
      (max_lis yes))))

(defun min_x_y (lis) ; lis of points
  (let ((xes (mapcar #'(lambda (point) (x point)) lis))
        (yes (mapcar #'(lambda (point) (y point)) lis)))
    (list
      (min_lis xes)
      (min_lis yes)))) 

(defun manhattan_distance (a b) ;mypoints
  "a and b are of type mypoint"
  (+
    (abs (- (x a) (x b)))
    (abs (- (y a) (y b)))))

(list 1 -3)









(defun rect (left right top bottom id)
  (list left right top bottom id))
(defun left (rect)
  (elt rect 0))
(defun right (rect)
  (elt rect 1))
(defun top (rect)
  (elt rect 2))
(defun bottom (rect)
  (elt rect 3))
(defun id (rect)
  (elt rect 4))



(defun rect_to_ordered_pairs (rect)
  (mapcar #'(lambda (y)
              (mapcar #'(lambda (x) (list x y))
                      (alexandria:iota (- (right rect) (left rect)) :start (left rect))))
          (alexandria:iota (- (top rect) (bottom rect)) :start (bottom rect))))






; ---------------------


(ql:quickload "alexandria")
(ql:quickload "regex")
(ql:system-apropos "regex")
;A claim like #123 @ 3,2: 5x4
; means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.

; part 1
(read_file "/home/justin/aoc_2018/day3/input/data.prep")
(setf alpha (read_file "/home/justin/aoc_2018/day3/input/data.prep"))
(setf tab (make-hash-table :test 'equal))
tab
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) tab)

(let ((total 0))
  (maphash #'(lambda (k v) (if (not (= 1 v)) (incf total))) tab)
  total)

(mapcar #'(lambda (rect) (clah rect tab)) (read_file "/home/justin/aoc_2018/day3/input/data.prep"))

(mapcar #'(lambda (rect) (clah_reject rect tab)) (read_file "/home/justin/aoc_2018/day3/input/smalldata.prep"))
;;;;;;;;;;;;;;;;

; part 2
(remove nil (beta alpha tab))


;;;;;;;;;;;;






(defun read_file (filename)
  (with-open-file (f filename :direction :input)
  (defun getlines(current_seq_of_lines)
        (let ((line (read f nil 'done nil)))
          (if (eq line 'done)
              current_seq_of_lines
              (getlines (append current_seq_of_lines (list line))))))
  (getlines '())))

;(regex:scan-str (regex:compile-str "are\([a-z]*\)") "hello theare sir. this is a house.")

(defun rect (left right top bottom id)
  (list left right top bottom id))
(defun left (rect)
  (elt rect 0))
(defun right (rect)
  (elt rect 1))
(defun top (rect)
  (elt rect 2))
(defun bottom (rect)
  (elt rect 3))
(defun id (rect)
  (elt rect 4))



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

    

(defun rect_to_ordered_pairs (rect)
  (mapcar #'(lambda (y)
              (mapcar #'(lambda (x) (list x y))
                      (alexandria:iota (- (right rect) (left rect)) :start (left rect))))
          (alexandria:iota (- (top rect) (bottom rect)) :start (bottom rect))))

(mapcar #'(lambda (ordered_pair) (incf_cbn (gethash ordered_pair tab))) (flatten (rect_to_ordered_pairs (car a))))
tab

(clah (car a) tab)
tab

(setf u nil)
(null u)
(incf_cbn u)
(defmacro incf_cbn (place)  ; a function would not work here. i needed incf_cbn to not eval place right away
  "incf could be null" ; but treat null like 0
  `(if (null ,place)
       (setf ,place 1)
       (incf ,place)))

(car alpha)
(rect_to_ordered_pairs (car alpha))


(defun beta (list_of_rects ht)
  (mapcar #'(lambda (a_rect)
              (if (reduce #'(lambda (a b) (and a b))
                          (clah_reject a_rect ht))
                  (id a_rect)))
          list_of_rects))

(defun clah_reject (rect ht)
  "like clah but return false when one of the square units were already occupied"
  (mapcar #'(lambda (ordered_pair) (if (= (gethash ordered_pair ht) 1)
                                      t
                                      nil))
                                       (flatten (rect_to_ordered_pairs rect))))

(defun clah (rect ht)
  (mapcar #'(lambda (ordered_pair) (incf_cbn (gethash ordered_pair ht)))    (flatten (rect_to_ordered_pairs rect))))

(defun flatten (lis)
  (if (and (listp lis)
           (not (null lis)))
      (if (listp (car lis))
          (append (car lis)         (flatten (cdr lis)))
          (append (list (car lis))  (flatten (cdr lis))))
      lis))




(mypoint_equal? (make-instance 'mypoint :x 1 :y 2)
       (make-instance 'mypoint :x 1 :y 1))
