#!/usr/bin/sbcl --script

;(setf f (open "/home/justin/aoc_2018/day2/input/data"))
(setf f '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab")

(setf ht (make-hash-table))

(setf (gethash #\a ht) 1) 

(map 'list #'identity "abcde")
(mapcar #'(lambda (x) x) (map 'list #'identity "abcde"))

(histo (map 'list #'identity "aaabcdebe"))
(justin  *)
(maphash #'(lambda (key val) (format t "~A:~A~%" key val)) ht)

(setf acc '())
acc
(append '(1 2) (list 55))


(defun justin (ht) 
  "returns a list with number of double and number of triples"
  (let ((two_count 0) (three_count 0))
    (maphash #'(lambda (key val) 
                 (if (= val 2)
                     (setf two_count   (+ two_count 1)))
                 (if (= val 3)
                     (setf three_count (+ three_count 1))))
             ht)
    (list two_count three_count)))

  
(defun histo (seq)
  (let ((ht (make-hash-table)))
  (mapcar #'(lambda (element)
              (multiple-value-bind (value anything?) (gethash element ht) 
                (if anything?
                    (setf (gethash element ht) (+ value 1))
                    (setf (gethash element ht) 1)))) ; could simplify and just -- if we see NIL for value make it 1
          seq)
  ht))




