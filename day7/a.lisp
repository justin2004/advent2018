(setf block_list (list
                     '(C A)
                     '(C F)
                     '(A B)
                     '(A D)
                     '(B E)
                     '(D E)
                     '(F E)))

(defun blocker (pair)
  (car pair))

(defun blockee (pair)
  (cadr pair))

(remove-duplicates
  (nodes_todo block_list))

(defun nodes_todo (lis)
  (let ((nodes '()))
    (mapcar #'(lambda (pair) 
                (mapcar #'(lambda (node)
                            (setf nodes (append nodes (list node))))
                        pair))
            lis)
  (remove-duplicates nodes)))

(is_blocked? 'a block_list)

(find_unblocked block_list (nodes_todo block_list))
(find_unblocked block_list (nodes_todo block_list))
(find_unblocked (remove 'C block_list :key #'blocker) (nodes_todo block_list))

(execute 'c (nodes_todo block_list) block_list)

(defun execute (node todo_list block_list)
  "execute node"
  (values
    (remove node todo_list)
    (remove node block_list :key #'blocker)))

; pass:
;   find_unblocked 
;   sort alphabetically
;   execute greatest ^



(defun is_blocked? (node block_list)
  (find node block_list :key #'blockee))

(defun find_unblocked (block_list todo_list) ; given a block_list return unblocked nodes from todo_list
  (remove nil 
          (remove-duplicates 
            (mapcar #'(lambda (node) 
                        (if (not (is_blocked? node block_list))
                            node))
                    todo_list))))


