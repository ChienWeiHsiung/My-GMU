(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
            (list (position 9 initial-puzzle-situation))) nil))

(defun create-random-state (num-moves)
    "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
    (let ((puzzle #(1 2 3 4 5 6 7 8 9 8)))
        (dotimes (x num-moves)
            (let ((moves (elt *valid-moves* (empty-slot puzzle))))
                (setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
        (build-state puzzle nil)))

(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)  
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)     
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot (puzzle)     
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 9))

(defun swap (pos1 pos2 puzzle)     
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves*      
    #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
        ,@body))

(defun make-move (move puzzle)
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (when (find move moves) (swap move (empty-slot puzzle) puzzle))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setf ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 9) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))



(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
  (let ((queue (make-empty-queue)) (history '()) (iterations 0) (state initial-state) ) ;;initalize variables
    (funcall enqueueing-function state queue)   ;;enqueue
    (setf history (list (puzzle-from-state state))) ;;add state to history
    (loop
      (incf iterations 1) ;;iterations ++
      (if (or (equalp iterations maximum-iterations) (eq (q-elements queue) nil)) ;;Failed condition
	  (return-from general-search 'FAILED))
      (setf state (remove-front queue)) ;;dequeue
      (if (funcall goal-test state) ;;Goal state condition
	  ;;if condition
	  (progn
	    (print iterations)
	    (return-from general-search state))
	  ;;else condition
	  (foreach-valid-move (move (puzzle-from-state state)) ;;for each child
			      (progn
				(let ((child (make-move move (puzzle-from-state state))) ) ;;for each child
				  (if (eq (check-in-history child history) nil) ;;"is not in history" condition
				      (progn
					(funcall enqueueing-function (build-state child state) queue) ;;enqueue child
					(push child history)))))))));;add child to history
  )

(defun goal-p (state)
    "Returns T if state is a goal state, else NIL.  Our goal test."
    (equalp (car state) #(1 2 3 4 5 6 7 8 9 8)) ;;compare a state to the goal state
)

(defun dfs-enqueuer (state queue)
    "Enqueues in depth-first order"
  (enqueue-at-front queue state) ;;enqueues in LIFO
)

(defun bfs-enqueuer (state queue)
  "Enqueues in breadth-first order"
  (enqueue-at-end queue state) ;;enqueues in FIFO
)                                         

(defun manhattan-enqueuer (state queue)
  "Enqueues by manhattan distance"
  (enqueue-by-priority queue #'f-manhattan state) ;;enqueues in f-manhattan
)

(defun num-out-enqueuer (state queue)
  "Enqueues by number of tiles out of place"
  (enqueue-by-priority queue #'f-num-out state)  ;;enqueues in f-num-out
  )

;;;; Below are the additional functions I added. 

;;check whether a puzzle is in history.
(defun check-in-history (child history)
  (dolist (puzzle history)
    (if (equalp puzzle child)
	(return-from check-in-history t)))
  nil
  )

;;h(n) for Manhattan. Compute Manhattan distance of a puzzle.
(defun manhattan (puzzle)
  (let ((sum 0))
    (dotimes (i (- (length puzzle) 1))
      (if (not (equalp (elt puzzle i) 9))
	  (progn
	    (incf sum (floor (/ (abs (- (elt puzzle i) (+ i 1))) 3)))
	    (incf sum (mod (abs (- (elt puzzle i) (+ i 1))) 3)))))
    sum)
  )
;;f(n) for Manhattan. Compute g(n) + h(n).
(defun f-manhattan (state)
  (+ (depth state) (manhattan (puzzle-from-state state)))
  )


;;h(n) for the number of tiles out of place. Compute total number of tiles out of place.
(defun num-out (puzzle)
  (let ((sum 0))
    (dotimes (i (- (length puzzle) 1))
      (if (and (not (equalp (elt puzzle i) 9)) (not (equalp (elt puzzle i) (+ i 1))))
	  (incf sum)))
    sum)
  )

;;f(n) for  the number of tiles out of place. Compute g(n) + h(n)
(defun f-num-out (state)
  (+ (depth state) (num-out (puzzle-from-state state)))
  )

#|
;;; The five test examples.

;;; Solves in 4 moves:
(setf s (make-initial-state '(
9 2 3
1 4 6
7 5 8)))

;;; Solves in 8 moves:
(setf s (make-initial-state '(
2 4 3
1 5 6
9 7 8)))

;;; Solves in 16 moves:
(setf s (make-initial-state '(
2 3 9
5 4 8
1 6 7)))

;;; Solves in 24 moves:
(setf s (make-initial-state '(
1 8 9
3 2 4
6 5 7)))

;;; easy or hard to solve?  Why?
(setf s (make-initial-state '(
9 2 3
4 5 6
7 8 1)))

|#

#|

;;;;;Answers for the Questions

1. Number of interations for  for the five examples above

  example  |    DFS   |    BFS   |  MANHATTAN  |  NUM-OUT
-----------|----------|----------|-------------|---------
     1     |  Failed  |     29   |       5     |      5
-----------|----------|----------|-------------|---------
     2     |  Failed  |    219   |      11     |     15
-----------|----------|----------|-------------|---------
     3     |  Failed  |   9582   |      92     |    413
-----------|----------|----------|-------------|---------
     4     |  Failed  |  Failed  |    1495     |  13464
-----------|----------|----------|-------------|---------
     5     |  Failed  |  Failed  |   Failed    | Failed


2. Actual solutions that MANHATTAN-ENQUEUER discovered

Example 1 :
 23
146
758

123
 46
758

123
4 6
758

123
456
7 8

123
456
78 

Example 2 :
243
156
 78

243
156
7 8

243
1 6
758

2 3
146
758

 23
146
758

123
 46
758

123
4 6
758

123
456
7 8

123
456
78 

Example 3 :
23 
548
167

2 3
548
167

243
5 8
167

243
568
1 7

243
568
17 

243
56 
178

243
5 6
178

243
 56
178

243
156
 78

243
156
7 8

243
1 6
758

2 3
146
758

 23
146
758

123
 46
758

123
4 6
758

123
456
7 8

123
456
78 


Example 4 :
18 
324
657

1 8
324
657

128
3 4
657

128
 34
657

 28
134
657

2 8
134
657

238
1 4
657

238
14 
657

23 
148
657

2 3
148
657

 23
148
657

123
 48
657

123
4 8
657

123
458
6 7

123
458
 67

123
 58
467

123
5 8
467

123
568
4 7

123
568
47 

123
56 
478

123
5 6
478

123
 56
478

123
456
 78

123
456
7 8

123
456
78 

|#

#|
;;;; Report

1. I added five additional functions  :
   check-in-history : check whether a puzzle is in history.
   manhattan : h(n) for Manhattan. Compute Manhattan distance of a puzzle.
   f-manhattan : f(n) for Manhattan. Compute g(n) + h(n).
   num-out : h(n) for the number of tiles out of place. Compute total number of tiles out of place.
   f-num-out : f(n) for  the number of tiles out of place. Compute g(n) + h(n)

2. Method of search and additional functions :
   i. check-in-history : Use dolist to loop over the history and compare specific puzzle to them.
  ii. manhattan : For example, 
                       1 8 3
                       9 6 5 
                       7 4 2
                 8 is currently at position 1 and it should be at position 7
                 The integer part of (7-1)/3 is the distance between y
                 And (7-1) mod 3 is the distance between x
                 So, the manhattan distance of 8 is the sum of them.
 iii. num-out : Use dotimes to compare current position to goal position of each element in puzzle.
  iv. general-search : Just follow the algorithm professor provided. And use macro "foreach-valid-move"
                       with function "make-move" to access children of a state.

3. A* with Manhattan or Num-out is better than BFS and DFS.
   Because A* search those states that seem to be closer to the goal state first.
   BFS and DFS just search in brute-force.
   Therefore, the performance of A* is better than those of BFS and DFS in example 1~5.


4. Why you believe the fifth example is (or isn't) difficult for MANHATTAN-ENQUEUER.

  In example 5, only 9 and 1 are at the wrong positions. 
  For each valid move, the h(n) increases because it moves a correct number to a wrong position.
  - initial state :
  9 2 3
  4 5 6   h(n) = 4
  7 8 1
  - valid move
  2 9 3
  4 5 6   h(n) = 5
  7 8 1
  - valid move
  4 2 3
  9 5 6   h(n) = 5
  7 8 1
  The h(n) of each state will be larger due to the messier puzzle.
  Also, the h(n) of each state in the same ply are the same.
  (At least the first few plies are the same, and I'm not sure about the deeper plies)
  So the f(n) of each state in the same ply are the same too.
  The Manhattan-enqueuer is the same as FIFO in at least the first few plies.
  Also, this puzzle is not solvable.

|#
