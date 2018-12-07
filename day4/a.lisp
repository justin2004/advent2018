(ql:system-apropos "pcre")
(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")
(defun read_file_into_seq_of_strings (filename)
  (with-open-file (f filename :direction :input)
  (defun getlines(current_seq_of_lines)
        (let ((line (read-line f nil 'done nil)))
          (if (eq line 'done)
              current_seq_of_lines
              (getlines (append current_seq_of_lines (list line))))))
  (getlines '())))

(setf a(read_file_into_seq_of_strings "/home/justin/aoc_2018/day4/input/data.sorted"))
(setf a (list "[1518-02-10 23:56] Guard #1487 begins shift" "[1518-02-11 00:14] falls asleep" "[1518-02-11 00:40] wakes up"))

(cl-ppcre:scan-to-strings "iGuard #\([0-9]*\)" (car a))
(type-of (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "Guard #\([0-9]*\)" (car a)) b))
(elt (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "Guard #\([0-9]*\)" (car a)) b) 0)

(cl-ppcre:scan-to-strings "^..............:\(..\).*asleep" (cadr a))
a
(multiple-value-bind (a b) (values 1) b)
(logruns_to_ a)
(mapcar #'
(blend (list 14 nil) (list nil 40))
(defun blend (sleep_lis wake_lis)
  (mapcar #'(lambda (asleep wake) (if (null asleep) wake asleep)) sleep_lis wake_lis))

(defun logruns_to_ (lis) ; expect lis to be a list of string starting with Guard begins followed by "falls" "wakes" in pairs
  ; returns lis like  guard_id sleep wake... 
  (let ((guard (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "Guard #\([0-9]*\)" (car lis)) b)))
    (append (list (elt guard 0))
    (blend
      (mapcar #'(lambda (string) (multiple-value-bind (a b)
                                   (cl-ppcre:scan-to-strings "^..............:\(..\).*asleep" string)
                                   (if (not (null a))
                                       (elt b 0))))
              (cdr lis))
      (mapcar #'(lambda (string) (multiple-value-bind (a b)
                                   (cl-ppcre:scan-to-strings "^..............:\(..\).*wakes" string)
                                   (if (not (null a))
                                       (elt b 0))))
              (cdr lis))))))
  
; 1487 14 39

(guard_id (cadddr a))
(sleep_or_wake_minute (cadddr a))

(defun sleep_or_wake_minute (string)
  (let ((minute (multiple-value-bind (a b)
                  (cl-ppcre:scan-to-strings "^..............:\(..\).*(asleep|wakes)" string) b)))
    (if (not (null minute))
        (elt minute 0)
        nil)))

(defun guard_id (string)
  (let ((guard (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "Guard #\([0-9]*\)" string) b)))
    (if (not (null guard))
        (elt guard 0)
        nil)))

(append nil (list 1 2))
(pp a)
(setf ht (make-hash-table :test #'equal))
(let ((current (gethash "33" ht))) (setf (gethash "33" ht) (append nil (list "id"))))
(gethash "33" ht)
(setf (gethash "33" ht) (append nil (list "id")))


(remove nil (mapcar #'prune_and_pairup (pp a)))
(car (remove nil (mapcar #'prune_and_pairup (pp a))))
(cdddr (elt (pp a) 44))
(prune_and_pairup (elt (pp a) 44))
; turn 1613 07 13 45 67    into 2 lists     1613 07 13     1613 45 67    or return it as it came in  -- returns a list of results
; and trim out 1613
(defun prune_and_pairup (lis)
  ;(format t "entering with ~A~%" lis)
  (cond 
    ((= (length lis) 1) 
     nil)
    ((= (length lis) 3)
     lis)
    ((> (length lis) 3)
     ; take first 2 and then...
     (list (list (car lis) (cadr lis) (caddr lis)) (prune_and_pairup (append (list (car lis)) (cdddr lis)))))))


(length (pp a))
(remove nil (remove-if #'(lambda (lis) (or (= (length lis) 1) (> (length lis) 3))) (pp a)))

(setf ht (make-hash-table))
ht
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) ht)

; load the hashtable
(mapcar #'(lambda (lis)
            (do ((i (parse-integer (cadr lis)) (+ 1 i)))
                ((= i (parse-integer (caddr lis))) 'done)
                (let ((current_lis (gethash i ht)))
                  (setf (gethash i ht) (append current_lis (list (car lis)))))))
        (remove nil (remove-if #'(lambda (lis) (or (= (length lis) 1) (> (length lis) 3))) (pp a))))

  
        
; i gave in and did this iteratively
(defun pp (lis)
  (let ((sofar '())
        (inprogress '()))
    (do ((i 0 (+ 1 i))) 
        ((= i (length lis)) 'done)
        (if (guard_id (elt lis i))
            ; build this shift's list
            (progn
              (setf sofar (append sofar (list inprogress)))
              (setf inprogress (list (guard_id (elt lis i)))))
            ; else
            (setf inprogress (append inprogress (list (sleep_or_wake_minute (elt lis i)))))))
  sofar))

        
a
(logruns a)
(defun logruns (lis)
  (let ((remaining '())
        (sofar '()))
    (defun bb (lis)
      "appends onto sofar until it hits another guard starting shift"
      (if (guard_id (car lis))
          (progn
            (setf remaining lis)
            nil)
          (append (list (sleep_or_wake_minute (car lis))) (bb (cdr lis)))))
    (setf sofar (append sofar (append (list (guard_id (car lis))) (bb (cdr lis)))))
    (if (null remaining)
        sofar
        (bb remaining))))

          
          
(defun events_not_shiftchange (lis)
  (if 

(logruns '(a s b))
