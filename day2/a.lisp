#!/usr/bin/sbcl --script

; part 1
(do_it (read_file_into_seq_of_strings "/home/justin/aoc_2018/day2/input/data"))

; part2
(part2 (read_file_into_seq_of_strings "/home/justin/aoc_2018/day2/input/data"))


(defun read_file_into_seq_of_strings (filename)
  (with-open-file (f filename :direction :input)
  (defun getlines(current_seq_of_lines)
        (let ((line (read-line f nil 'done nil)))
          (if (eq line 'done)
              current_seq_of_lines
              (getlines (append current_seq_of_lines (list line))))))
  (getlines '())))

(append '(a b) (list 1) (list 5 6))
  

              
(setf f '("abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"))
(setf j '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))

;convert "abc" into a (1 #\a) (2 #\b) (3 #\c)
((lambda (a b)  a) (map 'list #'identity "apple") (map 'list #'identity "apple"))
(mapcar #'(lambda (a b) (char= a b))  (map 'list #'identity "apple") (map 'list #'identity "appoe"))


(common_chars "apple" "appbe")

(defun common_chars (str1 str2)
  (let ((sofar  '()))
  (mapcar #'(lambda (a b)
              (if (char= a b)
                  (setf sofar (append sofar (list a)))))
          (map 'list #'identity str1) (map 'list #'identity str2))
  (map 'string #'identity sofar)))

j
(mapcar #'(lambda (str) (common_chars (elt j 0) str)) j)
(mapcar #'(lambda (str) (common_chars (elt j 1) str)) j)
; ...

(defun part2 (lis_of_strings)
  (let ((target_length (- (length (car lis_of_strings)) 1))) ; assuming all strings are same length
    (format t "target length is ~A~%" target_length)
    (remove nil
            (remove-duplicates (mapcar #'(lambda (str) (let ((len (length str)))
                                                         (if (= len target_length)
                                                             str)))
                                       (common_chars_cross lis_of_strings))
                               :test #'string=))))

(part2 jjj)
(length (remove-duplicates (common_chars_cross jjj)))


(defun common_chars_cross (lis_of_strings)
  (flatten
    (mapcar #'(lambda (str_outer)
                (mapcar #'(lambda (str_inner) 
                            (common_chars str_outer str_inner))
                        lis_of_strings))
            lis_of_strings)))

(defun flatten (lis)
  (if (and (listp lis)
           (not (null lis)))
      (if (listp (car lis))
          (append (car lis)         (flatten (cdr lis)))
          (append (list (car lis))  (flatten (cdr lis))))
      lis))
          
(setf a *)
(flatten a)


(defmacro one_per (lis)
  `(mapcar 



(map 'list #'identity "abcde") ; turn string to list of chars

(histo (map 'list #'identity "aaabcdebe"))
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) (histo (map 'list #'identity "aaabcdebe")))

(checksum "justinpj")

(defun checksum (string)
  (eval `(* ,@(ht_to_dub_trip_list (histo (map 'list #'identity string))))))

(mapcar #'(lambda (string) (ht_to_dub_trip_list (histo (map 'list #'identity string))))
        f)

(setf ff *)
ff

(do_it f)

(defun do_it (seq_of_strings)
  (let ((doubles_total 0) (triples_total 0))
    (mapcar #'(lambda (pair) (setf doubles_total (+ doubles_total (car pair))) (setf triples_total (+ triples_total (cadr pair))))
            (mapcar #'(lambda (string) (ht_to_dub_trip_list (histo (map 'list #'identity string))))
                    seq_of_strings))
    (* doubles_total triples_total)))


(defun ht_to_dub_trip_list (ht) 
  "returns a list with number of double and number of triples ... but with the comment below"
  (let ((two_count 0) (three_count 0))
    (maphash #'(lambda (key val) 
                 (if (and (= val 2)
                          (= two_count 0)) ; X doubles still just mean 1
                     (setf two_count   (+ two_count 1)))
                 (if (and (= val 3)
                          (= three_count 0)) ; X triples still just mean 1
                     (setf three_count (+ three_count 1))))
             ht)
    (list two_count three_count)))

  
(defun histo (seq)
  "create a histogram for the sequence"
  (let ((ht (make-hash-table)))
  (mapcar #'(lambda (element)
              (multiple-value-bind (value anything?) (gethash element ht) 
                (if anything?
                    (setf (gethash element ht) (+ value 1))
                    (setf (gethash element ht) 1)))) ; could simplify and just -- if we see NIL for value make it 1
          seq)
  ht))




(read_file_into_seq_of_strings "/home/justin/aoc_2018/day2/input/data")
