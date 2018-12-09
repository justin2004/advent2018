; TODO next day i must grow the functions from the bottom and don't let different representations stick around 

(mypoint_equal? (make-instance 'mypoint :x 1 :y 1)
       (make-instance 'mypoint :x 1 :y 1))

(defun flatten (lis)
  (if (and (listp lis)
           (not (null lis)))
      (if (listp (car lis))
          (append (car lis)         (flatten (cdr lis)))
          (append (list (car lis))  (flatten (cdr lis))))
      lis))

; test set
(setf prime_points
      (list
        (make-instance 'mypoint :x 1 :y -1)
        (make-instance 'mypoint :x 1 :y -6)
        (make-instance 'mypoint :x 8 :y -3)
        (make-instance 'mypoint :x 3 :y -4)
        (make-instance 'mypoint :x 5 :y -5)
        (make-instance 'mypoint :x 8 :y -9)))


(setf prime_points (list
                     (make-instance 'mypoint :x 192 :y -212)
                     (make-instance 'mypoint :x 294 :y -73)
                     (make-instance 'mypoint :x 153 :y -248)
                     (make-instance 'mypoint :x 238 :y -54)
                     (make-instance 'mypoint :x 354 :y -207)
                     (make-instance 'mypoint :x 269 :y -256)
                     (make-instance 'mypoint :x 155 :y -329)
                     (make-instance 'mypoint :x 132 :y -308)
                     (make-instance 'mypoint :x 211 :y -173)
                     (make-instance 'mypoint :x 261 :y -241)
                     (make-instance 'mypoint :x 300 :y -218)
                     (make-instance 'mypoint :x 143 :y -43)
                     (make-instance 'mypoint :x 226 :y -348)
                     (make-instance 'mypoint :x 148 :y -349)
                     (make-instance 'mypoint :x 114 :y -78)
                     (make-instance 'mypoint :x 77 :y -327)
                     (make-instance 'mypoint :x 140 :y -327)
                     (make-instance 'mypoint :x 202 :y -346)
                     (make-instance 'mypoint :x 174 :y -115)
                     (make-instance 'mypoint :x 86 :y -198)
                     (make-instance 'mypoint :x 132 :y -152)
                     (make-instance 'mypoint :x 167 :y -184)
                     (make-instance 'mypoint :x 146 :y -259)
                     (make-instance 'mypoint :x 277 :y -288)
                     (make-instance 'mypoint :x 330 :y -199)
                     (make-instance 'mypoint :x 98 :y -332)
                     (make-instance 'mypoint :x 290 :y -186)
                     (make-instance 'mypoint :x 322 :y -120)
                     (make-instance 'mypoint :x 295 :y -355)
                     (make-instance 'mypoint :x 346 :y -260)
                     (make-instance 'mypoint :x 305 :y -190)
                     (make-instance 'mypoint :x 294 :y -82)
                     (make-instance 'mypoint :x 156 :y -159)
                     (make-instance 'mypoint :x 114 :y -263)
                     (make-instance 'mypoint :x 340 :y -220)
                     (make-instance 'mypoint :x 353 :y -207)
                     (make-instance 'mypoint :x 220 :y -219)
                     (make-instance 'mypoint :x 152 :y -122)
                     (make-instance 'mypoint :x 223 :y -319)
                     (make-instance 'mypoint :x 236 :y -243)
                     (make-instance 'mypoint :x 358 :y -348)
                     (make-instance 'mypoint :x 174 :y -116)
                     (make-instance 'mypoint :x 306 :y -74)
                     (make-instance 'mypoint :x 70 :y -264)
                     (make-instance 'mypoint :x 352 :y -351)
                     (make-instance 'mypoint :x 194 :y -214)
                     (make-instance 'mypoint :x 153 :y -322)
                     (make-instance 'mypoint :x 225 :y -99)
                     (make-instance 'mypoint :x 237 :y -331)
                     (make-instance 'mypoint :x 279 :y -208)))



(defmethod print-object ((p mypoint) stream)
  (format stream "<x:~A y:~A>" (x p) (y p)))

; for each prime point count each in the ht
(setf melody
(mapcar #'(lambda (prime_point)
            (let ((total 0))
            (maphash #'(lambda (k v) 
                         (if (not (null v))
                           (if (mypoint_equal? (cadar v) prime_point)
                               (incf total))))
                     ht)
            (list total prime_point)))
        prime_points))

; and finally do this!
(sort melody #'< :key #'car)


; remove prime points that are closest to a point on the bounding rectangle



(find 55 (list 1 3 7 5 4))
(remove 4 (list 1 2 3 4 5 6 4 ))
(cancel_ties (list '(1 828282) '(2 8334) '(3 38438484) '(2 838383)))
(cancel_ties (list '(1 828282) '(2 8334) '(3 38438484) '(6 838383)))

; cancel ties in ht
(maphash #'(lambda (k v)
             (let ((current_val (gethash k ht)))
               (setf (gethash k ht) (cancel_ties current_val))))
         ht)

(defun cancel_ties (lis)
  "deletes values with tied distances or returns lists untouched "
  (let ((lis_cp lis))
  (do ((i 0 (+ 1 i)))
      ((= i (length lis)) 'done)
       (if (> (count (car (elt lis 0)) lis :key #'car) 1) ; only consider a tie if the best matches?
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

; first
(mapcar #'(lambda (op) (setf (gethash op ht) nil)) (bounding_rect_opairs prime_points))
;ht is loaded

;(beta ht a)


; for each of the regular points, loop thru the prime points, get the mdist btween them and sort and get the prime point with the lowest mdist
(maphash #'(lambda (regular_op closest_prime_op)
                     (setf (gethash regular_op ht)
                     (sort 
                       (mapcar #'(lambda (prime_pt)
                                   (list
                                     (manhattan_distance prime_pt (make-instance 'mypoint :x (car regular_op) :y (cadr regular_op)))
                                     prime_pt))
                               prime_points) #'< :key #'car)))
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
    (rect_to_ordered_pairs (bounding_rect lis)))) ; not using id

(maphash #'(lambda (k v)
             (if (point_on_rect? k (bounding_rect prime_points))
                 (format t "hit for ~A~%" k))
                 (format t "not hit for ~A~%" k) 
             )
         ht)

; remove from prime_points disqualified points -because they go to infinity-
(let ((tmp_prime_points prime_points))
  (maphash #'(lambda (k v)
               (if (point_on_rect? k (bounding_rect prime_points))
                   ; assuming v is already sorted.
                   ; now remove first point in the v list from consideration because
                   ; it might go to inifinity
                   (progn
                     (format t "found pt on bounding rect : ~A~%" k)
                     (if (not (null v))
                         (setf tmp_prime_points (remove (cadar v) tmp_prime_points :test #'mypoint_equal?))))  ; closet prime point to the op k
                   (format t "found pt not on bounding rect : ~A~%" k)
                   ))
           ht)
  tmp_prime_points)

; now set prime_points to what the above form evaulated to
(setf prime_points *)

(defun point_on_rect? (op rect) ; pt is an ordered pair      rect is rect
  (if
     (or
       (and
         (= (left rect) (car op))
         (<= (cadr op) (top rect))
         (>= (cadr op) (bottom rect)))

       (and
         (= (right rect) (car op))
         (<= (cadr op) (top rect))
         (>= (cadr op) (bottom rect)))

       (and
         (= (bottom rect) (cadr op))
         (<= (car op) (right rect))
         (>= (car op) (left rect)))    

       (and
         (= (top rect) (cadr op))
         (<= (car op) (right rect))
         (>= (car op) (left rect))))
     t))

(bounding_rect prime_points)

(defun bounding_rect (lis) ; lis of points
    (rect (car (min_x_y lis))
          (+ (car  (max_x_y lis)) 1)
          (+ (cadr (max_x_y lis)) 1)
          (cadr    (min_x_y lis))
          nil))
           
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


(rect_to_ordered_pairs (bounding_rect prime_points))

(defun rect_to_ordered_pairs (rect)
  (mapcar #'(lambda (y)
              (mapcar #'(lambda (x) (list x y))
                      (alexandria:iota (+ (- (right rect) (left rect)) 1) :start (left rect))))
          (alexandria:iota (+ (- (top rect) (bottom rect)) 1) :start (bottom rect))))







(ql:quickload "alexandria")








