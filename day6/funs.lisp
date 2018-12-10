(defun flatten (lis)
  (if (and (listp lis)
           (not (null lis)))
      (if (listp (car lis))
          (append (car lis)         (flatten (cdr lis)))
          (append (list (car lis))  (flatten (cdr lis))))
      lis))


(defclass mypoint ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))


(defmethod print-object ((p mypoint) stream)
  (format stream "<x:~A y:~A>" (x p) (y p)))


(defun cancel_ties (lis)
  "deletes values with tied distances or returns lists untouched "
  (let ((lis_cp lis))
  (do ((i 0 (+ 1 i)))
      ((= i (length lis)) 'done)
       (if (> (count (car (elt lis 0)) lis :key #'car) 1) ; only consider a tie if the best matches?
           (setf lis_cp nil)))
  lis_cp))
           

(defun remove_equals (lis)
  (count 2 lis :key #'(lambda (ele) (car ele))))



(defmethod mypoint_equal? ((a mypoint) (b mypoint))
  (and
    (= (x a) (x b))
    (= (y a) (y b))))
    


(defun max_lis (lis)
  (car (last (sort lis #'<))))

(defun min_lis (lis)
  (car (last (sort lis #'>))))

(defun avg_points (lis) ; lis of points
  (let ((xes (mapcar #'(lambda (point) (x point)) lis))
        (yes (mapcar #'(lambda (point) (y point)) lis)))
    (make-instance 'mypoint
                   :x
                   (/ (reduce #'+ xes) (length xes))
                   :y
                   (/ (reduce #'+ yes) (length yes)))))



(defun beta (ht point) ; ht contains   k: ordered pair   v: nearest prime ordered pair        point: one of the prime points
  (maphash #'(lambda (k v) 
               (format t "dist between point and this op ~A in ht ~A~%" 
                       k 
                       (manhattan_distance (make-instance 'mypoint :x (car k) :y (cadr k)) point)))
           ht))




(defun bounding_rect_opairs (lis) ; lis of points
"returns all the order pairs in the bounding rectangle for the lis of points "
  (flatten
    (rect_to_ordered_pairs (bounding_rect lis)))) ; not using id



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


(defun bounding_rect (lis) ; lis of points
    (rect (car (min_x_y lis))
          (+ (car  (max_x_y lis)) 1)
          (+ (cadr (max_x_y lis)) 1)
          (cadr    (min_x_y lis))
          nil))
           

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
                      (alexandria:iota (+ (- (right rect) (left rect)) 1) :start (left rect))))
          (alexandria:iota (+ (- (top rect) (bottom rect)) 1) :start (bottom rect))))




