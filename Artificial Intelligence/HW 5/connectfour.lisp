;;;; Connect 4 code
;;;; This code contains:
;;;;
;;;; (1) useful functions: COPY-ARRAY and MAX-ELEMENT (which is
;;;;     used in computer-make-move)
;;;;
;;;; (2) some constants: the self-explanatory BLACK and RED for
;;;;     the checkers of the two players, plus EMPTY meaning 
;;;;     no checker is in that location, plus MAX-WINS and MIN-WINS,
;;;;     which are the maximum and minimum possible scores that we'll
;;;;     be using, and are useful in alpha-beta.
;;;;
;;;; (3) Utilities for querying and setting the board, turns,
;;;;     remaining moves, the number of checkers in a row necessary
;;;;     to win, and the dimensions of the board.  Notice that some
;;;;     of these are special defuns of the form:
;;;;
;;;;     (defun foo () ... )
;;;;     (defun (setf foo) () ... )
;;;;
;;;;     The first one lets you do, of course, (foo ...)
;;;;     The second one lets you do (setf (foo ...) val)
;;;;
;;;; (4) Utilities for building a new game and copying a game state
;;;;
;;;; (5) Functions for determining valid moves, making moves, and
;;;;     iterating over all the moves available at a current
;;;;     game state
;;;;
;;;; (6) Functions for analyzing the game.  GAME-OVER tells you if
;;;;     the game state indicates a finished game.  
;;;;
;;;; (7) Functions for printing games and playing them as a human.
;;;;
;;;; (8) Functions for playing a human-human game or a human-computer
;;;;     game or a computer-computer game.



;;;;;;;;;;; UTILITIES

;; we use this to copy game boards
(defun copy-array (array)
  "Copies an Array"
  ;; from http://lemonodor.com/archives/000100.html
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :element-type (array-element-type array) :displaced-to array)
     dims)))


;; you might find this useful in your MAKE-COMPUTER-MOVE function (hint)
(defun max-element (elements value-function)
  "Returns the element from elements which, when passed into value-function,
returned the highest value.  If elements is nil, nil is returned.  In case
of ties, the later element is returned."
  (when elements
    (let ((max-element (first elements))
	  (max-element-score (funcall value-function (first elements))))
      (dolist (element (rest elements))
	(let ((element-score (funcall value-function element)))
	  (when (>= element-score max-element-score)
	    (setf max-element element)
	    (setf max-element-score element-score))))
      max-element)))


;;;;;;;;;;; CONSTANTS

(defconstant black 1 "A black piece, or the player 'black'")
(defconstant red -1 "A red piece, or the player 'red'")
(defconstant empty 0 "An empty square")
(defconstant max-wins 1000 "Max has won")
(defconstant min-wins -1000 "Min has won")



;;;;;;;;;;; THE GAME BOARD

;;; A GAME is defined as a list (board turn remaining-moves num-in-a-row)
;;; where TURN is either *black* or *red*, REMAINING-MOVES is
;;; the number of moves left to play, NUM-IN-A-ROW is the number of moves
;;; in a row necessary to win, and 
;;; BOARD is a two-dimensional array of integers, all initially *empty*.
;;; Pieces may be placed at any location <x,y> in the array as long
;;; as <x,y> doesn't already have a piece (duh) and
;;; either (1) y == 0 or (2) <x, y-1> already has a piece.  That is,
;;; gravity matters.

(defun board (game) (first game))
(defun (setf board) (val game) (setf (first game) val))

(defun turn (game) (second game))
(defun (setf turn) (val game) (setf (second game) val))
(defun toggle-turn (game) (setf (turn game) (- (turn game))))

(defun remaining-moves (game) (third game))
(defun (setf remaining-moves) (val game) (setf (third game) val))

(defun num-in-a-row (game) (fourth game))
(defun (setf num-in-a-row) (val game) (setf (fourth game) val))

(defun height (game) (array-dimension (board game) 1))
(defun width (game) (array-dimension (board game) 0))


(defun make-game (&optional (width 7) (height 6) (num-in-a-row 4))
  "Builds an empty game."
  (list (make-array (list width height) :element-type 'fixnum :initial-element empty)
	black
	(* width height)
	num-in-a-row))


(defun copy-game (game)
  "Copies a game, board and all."
  (list (copy-array (board game))
	(turn game)
	(remaining-moves game)
	(num-in-a-row game)))



;;;;;;;;;;; MOVES

;;; A MOVE is an integer representing a column in the board array.

(defun valid-move-p (move game) 
  "Returns t if the move is valid for this game"
  (= empty (aref (board game) move (1- (height game)))))

(defun row-for-move (move game) 
  "Returns the row that the checker would appear in if the given move were played."
  (dotimes (row (height game) -1)
    (if (= (aref (board game) move row) empty)
	(return row))))

(defun make-move (move game)
  "Makes a move on the game, returning a new game.  Nondestructive.  Does not check
to see if the move is valid or not.  If invalid, an error might occur."
  (let ((game (copy-game game)))
    (setf (aref (board game) move (row-for-move move game))
	  (turn game))
    (toggle-turn game)
    (decf (remaining-moves game))
    game))

(defmacro foreach-move ((move game) &rest body)
  "Iterates over each valid move position in the board."
  `(dotimes (,move (width ,game))
       (if (valid-move-p ,move ,game)
	   ,@body)))

(defun all-moves (game)
  "Returns a list of all games that can be played from this game position."
  (let (games)
    (foreach-move (move game)
		  (push (make-move move game) games))
    games))


;;;;;;;;;;; BOARD ANALYSIS

;;; The RESET-BOARD-COLUMNS function is private -- you shouldn't need to call it probably.
;;; The GAME-OVER function can be used either as a predicate (it returns non-nil if the
;;; game is over, else nil) or as a function which tells you WHO won or if there was a draw.
;;; This was an ugly function to write so I hope it doesn't have any bugs!  It's also not
;;; the fastest function in the west.  :-)



(defun reset-board-columns (game)
  "Private"
;; resets columns to proper red black values after being
;; messed around with by the diagonal checkers in the game-over function
  (let ((board (board game)))
    (dotimes (column (width game) game)
      (dotimes (row (height game))
	(if (<= (aref board column row) red)
	    (setf (aref board column row) red)
	    (if (>= (aref board column row) black)
		(setf (aref board column row) black)
		(return))))))) ;; bail

(defun game-over (game)
  "Returns 1 (black) if the game was won by black, -1 if the game was won by red,
0 if the game has drawn, and NIL if the game is NOT OVER YET"
  (let ((width (width game))
	(height (height game))
	(board (board game))
	(count)
	(num-in-a-row (num-in-a-row game)))

    ;; black vertical
    (dotimes (column width)
      (setf count 0)
      (dotimes (row height)
	(if (= empty (aref board column row)) (return)  ;; quick bail
	    (if (= black (aref board column row))
		(if (>= (incf count) num-in-a-row) (return-from game-over black))
		(setf count 0)))))

    ;; red vertical
    (dotimes (column width)
      (setf count 0)
      (dotimes (row height)
	(if (= empty (aref board column row)) (return)  ;; quick bail
	    (if (= red (aref board column row))
		(if (>= (incf count) num-in-a-row) (return-from game-over red))
		(setf count 0)))))

    ;; black horizontal
    (dotimes (row height)
      (setf count 0)
      (dotimes (column width)
	(if (= black (aref board column row))
	    (if (>= (incf count) num-in-a-row) (return-from game-over black))
	    (setf count 0))))

   ;; red horizontal
    (dotimes (row height)
      (setf count 0)
      (dotimes (column width)
	(if (= red (aref board column row))
	    (if (>= (incf count) num-in-a-row) (return-from game-over red))
	    (setf count 0))))

    ;; diagonal /
    (dotimes (column width)
      (if (> column 0)  ;; don't worry about first column
	  (dotimes (row height)
	    (if (> row 0) ;; don't worry about first row
		(if (= empty (aref board column row)) (return) ;; quick bail
		    (if (and (>= (aref board (1- column) (1- row)) black)
			     (= (aref board column row) black))
			(if (>= (incf (aref board column row)
				      (aref board (1- column) (1- row))) num-in-a-row)
			    (progn (reset-board-columns game) 
				   (return-from game-over black)))
			(if (and (<= (aref board (1- column) (1- row)) red)
				 (= (aref board column row) red))
			    (if (<= (incf (aref board column row)
					  (aref board (1- column) (1- row))) (- num-in-a-row))
				(progn (reset-board-columns game)
				       (return-from game-over red))))))))))
    (reset-board-columns game)

    ;; diagonal \
    (dotimes (column width)
      (if (> column 0)  ;; don't worry about first column
	  (dotimes (row height)
	    (if (< row (1- height)) ;; don't worry about LAST row
		(if (= empty (aref board column row)) (return) ;; quick bail
		    (if (and (>= (aref board (1- column) (1+ row)) black)
			     (= (aref board column row) black))
			(if (>= (incf (aref board column row)
				      (aref board (1- column) (1+ row))) num-in-a-row)
			    (progn (reset-board-columns game)
				   (return-from game-over black)))
			(if (and (<= (aref board (1- column) (1+ row)) red)
				 (= (aref board column row) red))
			    (if (<= (incf (aref board column row)
					  (aref board (1- column) (1+ row))) (- num-in-a-row))
				(progn (reset-board-columns game)
				       (return-from game-over red))))))))))

    ;; check for ties
    (reset-board-columns game)
    (if (= (remaining-moves game) 0) 0 nil)))




;;;;;;;;;;; HUMAN INTERFACE

(defun print-game (game)

  ;; this gimmick will handle up to 100 columns
  (terpri) (princ " ")
  (dotimes (column (width game))
    (princ (if (= (floor column 10) 0) #\space (floor column 10))))
  (terpri) (princ " ")
  (dotimes (column (width game))
    (princ (mod column 10)))

  ;; Now print the board
  (dotimes (r (height game))
    (terpri) (princ #\|)
    (let ((row (- (height game) r 1))) ;; print backwards
      (dotimes (column (width game))
	(let ((val (aref (board game) column row)))
	  (princ (if (= val black) #\@
		     (if (= val red) #\O #\space))))))
    (princ #\|))

  (terpri)(princ #\+)
  (dotimes (column (width game)) (princ #\-))
  (princ #\+)

  ;; Now print who's turn it is
  (format t "~%~a to play, ~a moves left, get ~a in a row" 
	  (if (= (turn game) black) "Black" "Red")
	  (remaining-moves game)
	  (num-in-a-row game)))


(defun make-human-move (game &optional depth)  ;; depth is ignored -- see tournament
  (declare (ignore depth))
  (loop
     (format t "~%~%Move to make -> ")
     (let ((move (read)))
       (if (and (numberp move) (>= move 0) (< move (width game)))
	   (if (valid-move-p move game)
	       (return (make-move move game))
	       (format t "~%Unavailable Move ~a, pick another!" move))
	   (format t "Seriously now.  Pick a number between ~a and ~a" 0 (1- (width game)))))))


(defun play-game (black-func red-func &key (black-depth 5) (red-depth nil) (verbose t) (width 10) (height 10) (num-in-a-row 4))
  "Plays a game between the two functions, passing in maximum
depths for each.  Optionally prints the board each move.  If red-depth is not
supplied, it is assumed to be equal to black-depth."
  (unless red-depth (setf red-depth black-depth))
  (let ((game (make-game width height num-in-a-row)))
    (loop
     (when verbose (print-game game))
     (let ((over (game-over game)))
       (when over
         (when verbose (format t (cond ((= over black) "~%~%Black Wins")
                                       ((= over red) "~%~%Red Wins")
                                       (t "~%~%Draw")))
           (return-from play-game over))))
     (setf game (funcall black-func game black-depth))
     (when verbose (print-game game))
     (let ((over (game-over game)))
       (when over
         (when verbose (format t (cond ((= over black) "~%~%Black Wins")
                                       ((= over red) "~%~%Red Wins")
                                       (t "~%~%Draw")))
           (return-from play-game over))))
     (setf game (funcall red-func game red-depth)))))

(defun play-human-human-game (&key (width 7) (height 6) (num-in-a-row 4))
       (play-game #'make-human-move #'make-human-move
		  :width width :height height :num-in-a-row num-in-a-row :verbose t))

(defun play-human-computer-game (func human-is-black &key (width 7) (height 6) (num-in-a-row 4) (computer-depth 4))
  (play-game (if human-is-black #'make-human-move func)
	     (if human-is-black func #'make-human-move)
	     :verbose t :width width :height height :num-in-a-row num-in-a-row
	     :black-depth computer-depth
	     :red-depth computer-depth))


;;; export some symbols so the individual evaluation packages can see them (for example, :sean-luke)

(export '(copy-array max-element black red empty max-wins min-wins
	  board turn toggle-turn remaining-moves num-in-a-row
	  height width make-game copy-game valid-move-p row-for-move
	  make-move foreach-move all-moves reset-board-columns game-over
	  print-game make-human-move play-game play-human-human-game
	  play-human-computer-game))
