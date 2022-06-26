(defun print-hash-table (ht)
	   (maphash (lambda (k v) (format t "~a: ~a~%" k v)) ht))

(defun check-q-table (q-table)
	   (maphash (lambda (state actions)
		      (maphash (lambda (action q-value)
				 (if (> q-value 0)
				     (format t "~a ~a: ~a~%" state action q-value)))
			       actions))
		    q-table))

(defun random-elt (sequence)
  "Returns a random element from a sequence"
  (elt sequence (random (length sequence))))

(defun num-states (q-table)
	   (hash-table-count q-table))

(defun num-actions (q-table)
	   (/ (hash-table-count (gethash (list 0 0 0) q-table)) 3))

(defun get-actions-map (num-actions)
	   (let ((actions-map (make-hash-table :test #'equalp)))
	     (loop for heap from 0 below 3
		   do (loop for action from 0 below num-actions
			    do (setf (gethash (list heap action) actions-map) 0)))
	     actions-map))

(defun make-q-table (num-states num-actions)
	   (let ((q-table (make-hash-table :test #'equalp)))
	     (setf num-states (+ 3 num-states))
	     (loop for heap1 from 0 below num-states
		   do (loop for heap2 from 0 below num-states
			    do (loop for heap3 from 0 below num-states
				     do (setf (gethash (list heap1 heap2 heap3) q-table) (get-actions-map num-actions)))))
	     q-table))

;; TESTING CODE START
(defparameter qt (make-q-table 3 3))
(setf (gethash (list 1 0) (gethash (list 0 0 0) qt)) 0.25)
;; TESTING CODE END

(defun max-q (q-table state)
	   (let* ((num-actions (num-actions q-table))
		  (best (gethash (list (1- 3) (1- num-actions)) (gethash state q-table))))
	     (maphash (lambda (action q-value)
			(declare (ignore action))
			(setf best (max q-value best)))
		      (gethash state q-table))
	     best))

(defun max-action (q-table state heap-size &optional val)
	   (let ((best (max-q q-table state))
		 bag)
	     (maphash (lambda (action q-value)
			(when (and (= q-value best)
				   (< (elt state (first action)) heap-size))
;;				   (<= (+ (elt state (first action)) (second action) 1) heap-size))
			  (push action bag)))
		      (gethash state q-table))
	     (if (and val (rest bag))
		 val
		 (random-elt bag))))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")

(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
	   (setf (gethash action (gethash current-state q-table))
		 (+ (* (- 1 (funcall alpha-func iteration)) (gethash action (gethash current-state q-table)))
		    (* (funcall alpha-func iteration) (+ reward (* gamma (max-q q-table next-state))))))
	   q-table)

(defun check-all-state (state compare-func heap-size)
	   (every (lambda (x-heap-size) (funcall compare-func x-heap-size heap-size)) state))

(defun learn-nim (heap-size gamma alpha-func num-iterations)
	   (let ((q-table (make-q-table heap-size 3))
		 state old-state reward my-move opponent-move)
	     (dotimes (i num-iterations)
	       (setf state (list 0 0 0))
	       (loop
		 (setf old-state state)
		 (setf my-move (max-action q-table state heap-size))
		 (setf (elt state (first my-move)) (+ (elt state (first my-move)) (second my-move) 1))
		 (setf reward 0)
		 (when (check-all-state state #'>= heap-size)
		   (setf reward -1))
		 (if (not (check-all-state state #'>= heap-size))
		     (progn
		       (setf opponent-move (max-action q-table state heap-size))
		       (setf (elt state (first opponent-move)) (+ (elt state (first opponent-move)) (second opponent-move) 1))
		       (when (check-all-state state #'>= heap-size)
			 (setf reward 1))))
		 (setf q-table (q-learner q-table reward old-state my-move state gamma alpha-func num-iterations))
		 (when (check-all-state state #'>= heap-size)
		   (return))))
	     q-table))

(defun ask-if-user-goes-first ()
	   "Returns true if the user wants to go first"
	   (y-or-n-p "Do you want to play first?"))

(defun make-user-move (state heap-size)
	   (let (heap sticks)
	     (loop
	       (format t "~%Take sticks from which heap? (~a-~a): " 1 3)
	       (setf heap (read))
	       (format t "Take how many sticks? (~a-~a): " 1 (min 3 (- heap-size (elt state(1-  heap)))) )
	       (setf sticks (read))
	       (when (and (numberp heap) (<= heap 3) (>= heap 1) (numberp sticks) (<= sticks (min 3 (- heap-size (elt state(1-  heap))))) (>= sticks 1))
		 (return (list heap sticks)))
	       (format t "~%Heap must be between 1 and 3. Sticks must be between 1 and ~a. Please try again.~%" (min 3 (- heap-size (elt state(1-  heap))))))))

(defun print-current-heap (heap-size state)
	   (format t "~%Current Heaps: ~a~%" (mapcar #'- (list heap-size heap-size heap-size) state)))

(defun play-nim (q-table heap-size)
	   (let ((answer (ask-if-user-goes-first))
		 (state (list 0 0 0))
		 user-move
		 computer-move)
	     (if answer
		 (progn
		   (loop
		     (print-current-heap heap-size state)
		     (setf user-move (make-user-move state heap-size))
		     (setf (elt state (1- (first user-move))) (+ (elt state (1- (first user-move))) (second user-move)))
		     (when (check-all-state state #'>= heap-size)
		       (format t "~%You Lose~%")
		       (return))
		     (setf computer-move (max-action q-table state heap-size))
		     (setf (elt state (first computer-move)) (+ (elt state (first computer-move)) (second computer-move) 1))
		     (format t "~%Computer removed ~a Sticks from Heap ~a" (1+ (second computer-move)) (1+ (first computer-move)))
		     (print-current-heap heap-size state)
		     (when (check-all-state state #'>= heap-size)
		       (format t "~%You Win~%")
		       (return))))
		 (progn
		   (loop
		     (print-current-heap heap-size state)
		     (setf computer-move (max-action q-table state heap-size))
		     (setf (elt state (first computer-move)) (+ (elt state (first computer-move)) (second computer-move) 1))
		     (format t "~%Computer removed ~a Sticks from Heap ~a" (1+ (second computer-move)) (1+ (first computer-move)))
		     (print-current-heap heap-size state)
		     (when (check-all-state state #'>= heap-size)
		       (format t "~%You Win~%")
		       (return))
		     (setf user-move (make-user-move state heap-size))
		     (setf (elt state (1- (first user-move))) (+ (elt state (1- (first user-move))) (second user-move)))
		     (when (check-all-state state #'>= heap-size)
		       (format t "~%You Lose~%")
		       (return)))))))

(defun best-actions (q-table heap-size)
	   (let (list)
	     (maphash (lambda (state actions)
			(declare (ignore actions))
			(push (max-action q-table state heap-size '(-)) list))
		      q-table)
	     (reverse list)))

(defparameter *heap-size* 10)
;; *HEAP-SIZE*

(defparameter *my-q-table* (learn-nim *heap-size* 0.1 #'basic-alpha 50000))
;; *MY-Q-TABLE*

(play-nim *my-q-table* *heap-size*)
;; Do you want to play first? (y or n) y

;; Current Heaps: (10 10 10)

;; Take sticks from which heap? (1-3): 2
;; Take how many sticks? (1-3): 3

;; Computer removed 3 Sticks from Heap 1
;; Current Heaps: (7 7 10)

;; Current Heaps: (7 7 10)

;; Take sticks from which heap? (1-3): 1
;; Take how many sticks? (1-3): 3

;; Computer removed 3 Sticks from Heap 2
;; Current Heaps: (4 4 10)

;; Current Heaps: (4 4 10)

;; Take sticks from which heap? (1-3): 2
;; Take how many sticks? (1-3): 2

;; Computer removed 1 Sticks from Heap 3
;; Current Heaps: (4 2 9)

;; Current Heaps: (4 2 9)

;; Take sticks from which heap? (1-3): 3
;; Take how many sticks? (1-3): 3

;; Computer removed 1 Sticks from Heap 1
;; Current Heaps: (3 2 6)

;; Current Heaps: (3 2 6)

;; Take sticks from which heap? (1-3): 2
;; Take how many sticks? (1-2): 2

;; Computer removed 3 Sticks from Heap 3
;; Current Heaps: (3 0 3)

;; Current Heaps: (3 0 3)

;; Take sticks from which heap? (1-3): 1
;; Take how many sticks? (1-3): 3

;; Computer removed 3 Sticks from Heap 3
;; Current Heaps: (0 0 0)

;; You Win