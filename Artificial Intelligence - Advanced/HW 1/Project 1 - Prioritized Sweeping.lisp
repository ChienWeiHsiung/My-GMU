;; PRIORITY QUEUE FUNCTIONS - START

(defun print-hash-table (ht)
	   (maphash (lambda (k v) (format t "~a: ~a~%" k v)) ht))

(defun enqueue-by-priority (pq key value)
	   (setf (gethash key pq)
		 (if (gethash key pq)
		     (append (gethash key pq) (list value))
		     (list value))))

(defun get-by-priority (pq key)
		(gethash key pq))

(defun dequeue-by-priority (pq key)
	   (let ((values (copy-seq (gethash key pq)))
		 item)
	     (if (consp values)
		 (prog2
		     (setf item (elt values 0))
		     (setf (gethash key pq) (rest values))))
	     item))

(defun dequeue-highest-priority (pq)
	   (let (max-priority)
	     (maphash (lambda (priority values)
			(if (and (or (null max-priority) (> priority max-priority)) (consp values))
			    (setf max-priority priority)))
		      pq)
	     (dequeue-by-priority pq max-priority)))

(defun is-priority-queue-not-empty (pq)
	   (let (is-empty)
	     (maphash (lambda (priority values) (declare (ignore priority)) (if (consp values) (setf is-empty t))) pq)
	     is-empty))

;; PRIORITY QUEUE FUNCTIONS - END

(defun random-elt (sequence)
	   "Returns a random element from a sequence"
	   (elt sequence (random (length sequence))))

(defun num-states (q-table)
	   "Returns the number of states in a q-table"
	   (first (array-dimensions q-table)))

(defun num-actions (q-table &optional state)
	   "Returns the number of actions in a q-table"
	   (declare (ignore state))
	   (second (array-dimensions q-table)))

(defun make-q-table (num-states num-actions)
	   "Makes a q-table, with initial values all set to 0"
	   (make-array (list num-states num-actions) :initial-element 0))

(defun max-q (q-table state) 
	   "Returns the highest q-value for a given state over all possible actions. If the state is outside the range, then utility-for-outside-state-range is returned."
	   (let* ((num-actions (num-actions q-table))
		  (best (aref q-table state (1- num-actions))))
	     (dotimes (action (1- num-actions) best)
	       (setf best (max (aref q-table state action) best)))))

(defun max-action (q-table state &optional val)
	   "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random; else val is returned instead when there's a tie. If state is outside the range, then an error is generated
 (probably array-out-of-bounds)."
	   (let ((num-actions (num-actions q-table))
		 (best (max-q q-table state))
		 bag)
	     (dotimes (action num-actions)
	       (when (= (aref q-table state action) best)
		 (push action bag)))
	     (if (and val (rest bag))
		 val
		 (random-elt bag))))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")

(defun basic-alpha (iteration)
	   (declare (ignore iteration))
	   *basic-alpha*)

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
	   "Modifies the q-table and returns it.  alpha-func is a function which must be called to provide the current alpha value."
	   (setf (aref q-table current-state action)
		 (+ (* (- 1 (funcall alpha-func iteration)) (aref q-table current-state action))
		    (* (funcall alpha-func iteration) (+ reward (* gamma (max-q q-table next-state))))))
	   q-table)

(defun prioritized-sweeping (q-table pq model gamma alpha-func num-iterations sweep-iterations threshold)
	   (dotimes (i sweep-iterations)
	     (if (not (is-priority-queue-not-empty pq))
		 (return))
	     (let* ((state-data (dequeue-highest-priority pq))
		    (new-state-data (gethash state-data model))
		    (state (first state-data))
		    (my-move (second state-data))
		    (new-state (first new-state-data))
		    (reward (second new-state-data))
		    (num-actions (num-actions q-table))
		    p)
	       (q-learner q-table reward state my-move new-state gamma alpha-func num-iterations)
	       (loop for previous-action from 0 below num-actions
		     for previous-state = (- state previous-action 1)
		     for possible-state-data = (gethash (list previous-state previous-action) model)
		     do (if (and (consp possible-state-data) (equalp (first possible-state-data) state))
			    (prog2
				(setf p (+ (second possible-state-data) (* gamma (max-q q-table my-move)) (- (aref q-table previous-state previous-action))))
				(if (and (not (null p)) (> p threshold))
				    (enqueue-by-priority pq p (list previous-state previous-action)))))))))

(defun learn-nim (heap-size gamma alpha-func num-iterations sweep-iterations threshold)
	   (let ((q-table (make-q-table (+ heap-size 6) 3))
		 (pq (make-hash-table :test #'equalp))
		 (model (make-hash-table :test #'equalp))
		 state old-state reward my-move p
		 )
	     (dotimes (i num-iterations)
	       (setf state 0)
	       (loop
		 (setf old-state state)
		 (setf my-move (max-action q-table state))
		 (setf state (+ state (+ 1 my-move)))
		 (setf reward 0)
		 (when (= state heap-size)
		   (setf reward -1))
		 (setf state (+ state (+ 1 (max-action q-table state))))
		 (when (= state heap-size)
		   (setf reward 1))
		 (when (> state heap-size)
		   (return))
		 (setf (gethash (list old-state my-move) model) (list state reward))
		 (setf p (+ reward (* gamma (max-q q-table state)) (- (aref q-table old-state my-move))))
		 (if (> p threshold)
		     (enqueue-by-priority pq p (list old-state my-move)))
		 (prioritized-sweeping q-table pq model gamma alpha-func num-iterations sweep-iterations threshold)))
	     q-table))

(defun ask-if-user-goes-first ()
	   "Returns true if the user wants to go first"
	   (y-or-n-p "Do you want to play first?"))

(defun make-user-move ()
	   "Returns the number of sticks the user wants to remove"
	   (let ((result))
	     (loop
	       (format t "~%Take how many sticks?  ")
	       (setf result (read))
	       (when (and (numberp result) (<= result 3) (>= result 1))
		 (return result))
	       (format t "~%Answer must be between 1 and 3"))))

(defun play-nim (q-table heap-size)
	   "Plays a game of nim.  Asks if the user wants to play first, then has the user play back and forth with the game until one of them wins.  Reports the winner."
	   (format t "~%Initial number of sticks: ~a~%~%" heap-size)
	   (let ( (answer (ask-if-user-goes-first)) (state 0) computer-action)
	     (if answer
		 (progn
		   (loop
		     (format t "~%Sticks left : ~d" (- heap-size state))
		     (setf state (+ state (make-user-move)))
		     (when (> state (- heap-size 1))
		       (format t "~%You Lose")
		       (return))
		     (setf computer-action (+ 1 (max-action q-table state)))
		     (setf state (+ state computer-action))
		     (format t "~%Computer removed sticks: ~d" computer-action)
		     (when (> state (- heap-size 1))
		       (format t "~%You Win")
		       (return))))
		 (progn
		   (loop
		     (setf computer-action (+ 1 (max-action q-table state)))
		     (setf state (+ state computer-action))
		     (format t "~%Computer removed sticks: ~d" computer-action)
		     (when (> state (- heap-size 1))
		       (format t "~%You Win")
		       (return))
		     (format t "~%Sticks left : ~d" (- heap-size state))
		     (setf state (+ state (make-user-move)))
		     (when (> state (- heap-size 1))
		       (format t "~%You Lose")
		       (return)))))))

(defun best-actions (q-table)
	   "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
	   (let ((list))
	     (dotimes (i (- (num-states q-table) 6))
	       (push (max-action q-table i '(-)) list))
	     (reverse list)))

(defparameter *my-q-table* (learn-nim 22 0.1 #'basic-alpha 50000 10000 0.0001))
;; *MY-Q-TABLE*

(best-actions *my-q-table*)
;; ((-) (-) (-) (-) (-) 2 1 1 1 1 1 0 1 (-) 2 (-) 1 (-) 2 1 0 (-))

(play-nim *my-q-table* 22)
;; Initial number of sticks: 22

;; Do you want to play first? (y or n) n

;; Computer removed sticks: 3
;; Sticks left : 19
;; Take how many sticks?  3

;; Computer removed sticks: 2
;; Sticks left : 14
;; Take how many sticks?  1

;; Computer removed sticks: 2
;; Sticks left : 11
;; Take how many sticks?  3

;; Computer removed sticks: 3
;; Sticks left : 5
;; Take how many sticks?  2

;; Computer removed sticks: 2
;; Sticks left : 1
;; Take how many sticks?  1

;; You Lose