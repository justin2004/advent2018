#!/usr/bin/sbcl --script


(defun accum (stream)
  (defun accum_ (state)
    (let ((line (read-line stream nil 'done nil)))
      ;(format t "read a:~A~%" line)
      (if (eq line 'done) 
          state
          (accum_ (+ state (parse-integer line))))))
  (accum_ 0))


;(format t "~A~%" (accum *standard-input*))


; original
(defun accum_and_seen (filename)
  (defun accum_ (seen_lis state stream)
    (let ((line (read-line stream nil 'done nil)))
      (if (find state seen_lis)
          (progn
            (format t "seen ~A twice~%" state)
            state)
          (if (eq line 'done) 
              (progn
                (close stream)
                (format t "length of seen_lis: ~A. opening file again~%" (length seen_lis))
                ; didn't get a dataum this time -- just reopen the file for the next pass
                (accum_ seen_lis state (open filename :direction :input)))
              (accum_   (append seen_lis (list state))  (+ state (parse-integer line)) stream)))))
  (accum_ '()  0 (open filename :direction :input)))

; faster
(defun accum_and_seen (filename)
  (defun accum_ (seen_lis state stream ht) ; not really using seen_lis anymore
    (let ((line (read-line stream nil 'done nil)))
      (if (gethash state ht)
          (progn
            (format t "seen ~A twice~%" state)
            state)
          (if (eq line 'done) 
              (progn
                (close stream)
                (format t "length of seen_lis: ~A. opening file again~%" (hash-table-count ht))
                ; didn't get a dataum this time -- just reopen the file for the next pass
                (accum_ seen_lis state (open filename :direction :input) ht))
              (accum_   (setf (gethash line ht) t)  (+ state (parse-integer line)) stream ht)))))    
  (accum_ '()  0 (open filename :direction :input) (make-hash-table)))
  
;(format t "~A~%" (accum_and_seen "/home/justin/advent2018/input/data")) 
(format t "~A~%" (accum_and_seen "/home/justin/advent2018/input/sachin"))
;(format t "~A~%" (accum_and_seen "testdata"))
