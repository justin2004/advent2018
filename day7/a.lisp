(setf block_list (list
'(D T)
'(T K)
'(O S)
'(Y G)
'(F Q)
'(Q C)
'(R X)
'(E X)
'(V U)
'(U H)
'(X S)
'(L C)
'(P B)
'(A N)
'(K W)
'(H Z)
'(W C)
'(M J)
'(S J)
'(G I)
'(Z I)
'(C N)
'(B J)
'(J N)
'(I N)
'(O L)
'(Z B)
'(F Z)
'(J I)
'(Y R)
'(Q N)
'(U M)
'(V C)
'(W B)
'(T E)
'(Q W)
'(A J)
'(G Z)
'(V B)
'(E B)
'(R J)
'(W J)
'(H N)
'(G B)
'(U X)
'(Y M)
'(B I)
'(V I)
'(S C)
'(F K)
'(X G)
'(M C)
'(U J)
'(X H)
'(L A)
'(A K)
'(V G)
'(E U)
'(P I)
'(P G)
'(A I)
'(Y J)
'(X K)
'(B N)
'(A M)
'(E K)
'(S N)
'(Q A)
'(L W)
'(F A)
'(K I)
'(M N)
'(O P)
'(L J)
'(Z N)
'(Q U)
'(V W)
'(F P)
'(F M)
'(E Z)
'(T L)
'(F C)
'(M G)
'(Y S)
'(C J)
'(U I)
'(Y W)
'(Z C)
'(Y N)
'(Q P)
'(P K)
'(O M)
'(W S)
'(M B)
'(X C)
'(D C)
'(H J)
'(S G)
'(C I)
'(Y V)
'(F I)))


;test data
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


(pass (nodes_todo block_list) block_list)
; pass:
;   find_unblocked 
;   sort alphabetically
;   execute greatest ^
(defun pass (todo_list block_list)
  (if (not (null todo_list))
  (multiple-value-bind (next_todo_list next_block_list)
    (execute 
      (let ((node_to_execute
              (car 
                (sort (find_unblocked block_list todo_list)
                      #'(lambda (a b) (string<  (symbol-name a) (symbol-name b)))))))
        (format t "~A" node_to_execute) ; execute sequence
        node_to_execute)
      todo_list 
      block_list)

    (pass next_todo_list next_block_list))))



(defun is_blocked? (node block_list)
  (find node block_list :key #'blockee))

(defun find_unblocked (block_list todo_list) ; given a block_list return unblocked nodes from todo_list
  (remove nil 
          (remove-duplicates 
            (mapcar #'(lambda (node) 
                        (if (not (is_blocked? node block_list))
                            node))
                    todo_list))))


