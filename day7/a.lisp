; note -  i am trying to do with minimal side effects -- assignments -- 
;     but how valuable is that really? TODO
;    it does slow me down because instead of just making an assignment and using it / moving it around later
;    i try to hold state by...

; matching paren highlighting ... if you hit a paren with your cursor that is in a comment it is like an error trap :/

; is there any problem with, just inside a defun, just opening a let and declaring a bunch of variables?

; i am making this hard than it needs to be by not having a principle way of using lets, multiple-value-binds, etc.
; i've got scopes willy nilly
; i need to get to the bottom of that or else just use c or try something else


(load "/home/justin/aoc_2018/day6/funs.lisp") ; for min_lis
(ql:quickload "alexandria")
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
  "execute node from todo_list and then remove node from block_list 
   returns 2 values: new todolist and new blocklist"
  ; though in pass_with_time node will already be removed from todo_list
  (values
    (remove node todo_list)
    (remove node block_list :key #'blocker)))

(defun execute_part2 (node todo_list block_list)
  "execute node from todo_list and then remove node from block_list 
   returns 1 values:  new blocklist"
  ; though in pass_with_time node will already be removed from todo_list
  (remove node block_list :key #'blocker))


; initialize workers  -- i guess i am keeping the workers global
(defun get_node (lis) 
  (car lis))
(defun get_time_left (lis)
  (cadr lis))
(setf workers (make-hash-table))  ; k worker_id       v list: the node it is currently working on and how much time is left
(mapcar #'(lambda (worker_id) (setf (gethash worker_id workers) nil)) (alexandria:iota 5))
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) workers)


(defun get_free_worker (workers)
  "returns an id of a free worker if one exists"
  (let ((free_worker_id nil))
  (maphash #'(lambda (k v)
               (if (null v)
                   (setf free_worker_id k))) ; TODO could break out now
           workers)
  free_worker_id))

(get_time_cost 'A)
(get_time_cost 'Z)

(defun get_time_cost (node)
  (+ 1 60    ; add 60 now for the real thing
  (abs
    (-
      (char-int (elt (symbol-name 'A)   0))
      (char-int (elt (symbol-name node) 0))))))

(attempt_to_pair (list 'A 'H) workers)

(defun attempt_to_pair (nodes workers) ; nodes is a list of the nodes that can be paired and workers is the available workers
  ; and return a list of the nodes that were paired 
  (mapcar #'(lambda (node)
              (let ((free_worker_id
                      (get_free_worker workers)))
                (if (not (null free_worker_id))
                    ; do the pairing
                    (progn
                      (setf (gethash free_worker_id workers) (list node (get_time_cost node))) ; side effect
                      node)))) ; but return this
          nodes))

block_list
(pass_with_time (nodes_todo block_list) block_list workers 0)
; 214 was too low   ahh, i forgot to add the 60 seconds per

; pass_with_time -- needs to accept args: for each worker node-> in progress node and how much time is left on it
;   find_unblocked
;   sort alphabetically 
;   pair nodes with free workers
;     pairing a node means the node is no longer in the todo_nodelist but it is still in the blocklist
;   tick until a node is ready to execute -- execution happens instantly --
;   execute that/those nodes ^   
;   call pass_with_time with remaining
(defun pass_with_time (todo_list block_list workers_state time_passed_so_far)
  (format t "~%staring pass_with_time - todo_list:~A, block_list:~A, time_passed_so_far:~A~%"
          todo_list
          block_list
          time_passed_so_far)
  ; worker state
  (format t "worker state:~%")
  (maphash #'(lambda (k v) (format t "~A:~A~%" k v)) workers)
  (if (not (null todo_list))

      ;get unblocked and sort them
      (let ((next_block_list block_list) ; we'll just overwrite this as we go
            (next_todo_list todo_list) ; start with a full list and then we will remove nodes once they are executed
            (nodes_that_can_start_ticking
              (sort (find_unblocked block_list todo_list)
                    #'(lambda (a b) (string<  (symbol-name a) (symbol-name b))))))
        (format t "could start ~A" nodes_that_can_start_ticking) ; execute sequence
        ; do pairing
        (let ((successfully_paired_nodes
                (attempt_to_pair nodes_that_can_start_ticking workers))) ; TODO relying on global workers
          (format t "we paired ~A" successfully_paired_nodes)
          ; so now remove the paired ones from the todo_nodelist
          (mapcar #'(lambda (node)
                      (setf next_todo_list (remove node next_todo_list)))
                  successfully_paired_nodes)
          (multiple-value-bind (finished_nodes time_passed)
            ; bind call
            (tick_workers workers)
            ; body
            (format t "time passed:~A, finished nodes:~A~%" time_passed finished_nodes)
            (mapcar #'(lambda (node) ; returns new blocklist but we already set it inside
                        (setf next_block_list
                              (execute_part2 node next_todo_list next_block_list)))
                    finished_nodes)
            (format t "at the end next_block_list:~A~%next_todo_list:~A, time_passed:~A~%" next_block_list next_todo_list time_passed)
            (pass_with_time next_todo_list next_block_list workers (+ time_passed_so_far time_passed))
            )))))





(min_lis (list 1  nil 5))
(tick_workers workers)
(setf workers (make-hash-table))  ; k worker_id       v list: the node it is currently working on and how much time is left
(mapcar #'(lambda (worker_id) (setf (gethash worker_id workers) nil)) (alexandria:iota 5))
(maphash #'(lambda (k v) (format t "~A:~A~%" k v)) workers)
(attempt_to_pair (list 'A 'B 'H) workers)

(defun tick_workers (workers)
  ; workers can finish simultaneously
  "tick until a worker is done and return the finished nodes in a list and how much time passed"
  ; also update workers globally
  (let ((times_left  '())
        (nodes_ready '())
        (gtime_ticked_here 0))
    (maphash #'(lambda (k v) 
                 (setf times_left (remove nil (append times_left (list (get_time_left v))))))
             workers)
    (let ((time_ticked_here (min_lis times_left)))
      (setf gtime_ticked_here time_ticked_here) ;  ek TODO 
      ;(format t "time_ticked_here is ~A~%" time_ticked_here)
      ; 
      (maphash #'(lambda (k v) 
                   (if (and (not (null v)) 
                            (= (get_time_left v)  time_ticked_here))
                       (progn
                         ; then node k is ready to execute
                         (setf nodes_ready (append nodes_ready (list (get_node v))))
                         ; also free up that worker
                         (setf (gethash k workers) nil))))
               workers)
      ; now deduct the time_ticked_here from all active workers   TODO maybe i could do this in the above maphash
      (maphash #'(lambda (k v) 
                   (if (not (null v))
                       (setf (gethash k workers) (list (get_node v) (- (get_time_left v) time_ticked_here)))))
               workers))
    (values 
      nodes_ready
      gtime_ticked_here)
    ))


    




(pass (nodes_todo block_list) block_list)
; pass:
;   find_unblocked 
;   sort alphabetically
;   execute greatest ^
;   call pass with remaining todolist
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


