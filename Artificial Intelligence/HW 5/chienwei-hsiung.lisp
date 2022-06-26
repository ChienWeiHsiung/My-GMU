(defpackage :chienwei-hsiung 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :chienwei-hsiung)

(defun alpha-beta (game current-depth max-depth
		   is-maxs-turn-p expand terminal-p evaluate
		   alpha beta)

  (if (or  (funcall terminal-p game) (>= current-depth max-depth) )    ;; if
      (return-from alpha-beta (funcall evaluate game is-maxs-turn-p))
      (if (funcall is-maxs-turn-p game) ;; else if
	  (progn
	    (dolist (child (funcall expand game))
	      (let ((mm (alpha-beta child (+ 1 current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta)))
		(setf alpha (max alpha mm))
		(if (>= alpha beta)
		    (return-from alpha-beta beta))))
	    (return-from alpha-beta alpha))
	  (progn  ;; else
	    (dolist (child (funcall expand game))
	      (let ((mm (alpha-beta child (+ 1 current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta)))
		(setf beta (min beta mm))
		(if (>= alpha beta)
		    (return-from alpha-beta alpha))))
	    (return-from alpha-beta beta))))
)



(defun evaluate (game is-maxs-turn-p)
  "Returns an evaluation, between min-wins and max-wins inclusive, for the game.
is-maxs-turn-p is a function which, when called and passed the game, returns true
if it's max's turn to play."
  (let ((end (game-over game)))
    (if (null end) ;; game not over yet
	(progn
	  ;;score function
	  (get-score game)
	  )
	(if (= 0 end)  ;; game is a draw
	    0
	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1))))))

;;;;;
;;;;;Additional function

;;;compute a score
;;;I count the number of 1 2 3 connected row for black and red. score for each connected row depending on the num-in-a-row
;;;And then, the sum of black minus the sum of red = score
(defun get-score (game)
  (let ((width (width game)) (height (height game)) (board (board game)) (count) (score)  (num-in-a-row (num-in-a-row game)) (exp-base))
    (setf exp-base (expt 10 (/ (log max-wins 10) num-in-a-row))) ;;base of exponent, if 2 connected row, score will be exp-base^2
    (setf score 0)
    
    ;; (black) find if there is connected row in horizontal direction 
    (dotimes (row height)
      (setf count 0)
      (dotimes (column width)
	(if (= black (aref board column row))
	    (incf count)
	    (if (> count 0) ;;count > 0, means there is a connected row
		(progn 
		  (incf score (score-horizontal board (- column 1) row count exp-base))
		  (setf count 0)))))
      (if (> count 0) 
	  (incf score (score-horizontal board (- width 1) row count exp-base))))

    ;; (black) find if there is connected row in vertical direction 
    (dotimes (column width)
      (setf count 0)
      (dotimes (row height)
	(if (= black (aref board column row))
	    (incf count)
	    (if (> count 0) 
		(progn
		  (incf score (score-vertical board column (- row 1) count exp-base))
		  (setf count 0))))	
	(if (= empty (aref board column row)) (return))))

    ;; (black) find if there is connected row in diagonal/ direction 
    (setf count 0)
    (dotimes (column width)
      (dotimes (row height)
	(if (= empty (aref board column row)) (return)) 
	(if (= (aref board column row) black) 
	    (progn
	      (dotimes (i (1- num-in-a-row)) 
		(if ( and (< (+ column i) width) (< (+ row i) height) ) 
		    (if (= (aref board (+ column i) (+ row i)) black)
			(progn
			  (incf (aref board (+ column i) (+ row i)) 1) 
			  (incf count 1))
			(return))))
	      (if (> count 0) 
		  (progn
		    (incf score (score-diagonal-up board column row count exp-base))
		    (setf count 0)))))))
    (reset-board-columns game)

    ;; (black) find if there is connected row in diagonal\ direction 
    (setf count 0)
    (dotimes (column width)
      (dotimes (row height)
	(if (= empty (aref board column row)) (return))
	(if (= (aref board column row) black)
	    (progn
	      (dotimes (i (1- num-in-a-row))
		(if ( and (< (+ column i) width) (>= (- row i) 0) )
		    (if (= (aref board (+ column i) (- row i)) black)
			(progn
			  (incf (aref board (+ column i) (- row i)) 1)
			  (incf count 1))
			(return))))
	      (if (> count 0)
		  (progn
		    (incf score (score-diagonal-down board column row count exp-base))
		    (setf count 0)))))))
    (reset-board-columns game)

    ;; (red) find if there is connected row in horizontal direction 
    (dotimes (row height)
      (setf count 0)
      (dotimes (column width)
	(if (= red (aref board column row))
	    (incf count)
	    (if (> count 0) 
		(progn 
		  (decf score (score-horizontal board (- column 1) row count exp-base))
		  (setf count 0)))))
      (if (> count 0)
	  (decf score (score-horizontal board (- width 1) row count exp-base))))

    ;; (red) find if there is connected row in vertical direction
    (dotimes (column width)
      (setf count 0)
      (dotimes (row height)
	(if (= red (aref board column row))
	    (incf count)
	    (if (> count 0) 
		(progn 
		  (decf score (score-vertical board column (- row 1) count exp-base))
		  (setf count 0))))	
	(if (= empty (aref board column row)) (return))))

    ;; (red) find if there is connected row in diagonal/ direction
    (setf count 0)
    (dotimes (column width)
      (dotimes (row height)
	(if (= empty (aref board column row)) (return)) 
	(if (= (aref board column row) red)
	    (progn
	      (dotimes (i (1- num-in-a-row)) 
		(if ( and (< (+ column i) width) (< (+ row i) height) ) 
		    (if (= (aref board (+ column i) (+ row i)) red)
			(progn
			  (decf (aref board (+ column i) (+ row i)) 1) 
			  (incf count 1))
			(return))))
	      (if (> count 0) 
		  (progn
		    (decf score (score-diagonal-up board column row count exp-base))
		    (setf count 0)))))))
    (reset-board-columns game)

    ;; (red) find if there is connected row in diagonal\ direction
    (setf count 0)
    (dotimes (column width)
      (dotimes (row height)
	(if (= empty (aref board column row)) (return))
	(if (= (aref board column row) red)
	    (progn
	      (dotimes (i (1- num-in-a-row))
		(if ( and (< (+ column i) width) (>= (- row i) 0) )
		    (if (= (aref board (+ column i) (- row i)) red)
			(progn
			  (decf (aref board (+ column i) (- row i)) 1)
			  (incf count 1))
			(return))))
	      (if (> count 0)
		  (progn
		    (decf score (score-diagonal-down board column row count exp-base))
		    (setf count 0)))))))
    (reset-board-columns game)

    score
    )
  )

;;compute the score of a connected row (diagonal\), check if it's a valid row (valid : at least one side of the connected row can be placed a corresponding color)
(defun score-diagonal-down (board column row count exp-base)
  (if ( and (< (+ column count) (array-dimension board 0) ) (>= (- row count) 0) )  ;;bottom right
      (if (= (aref board (+ column count) (- row count)) empty) 
	  (if (= (- row count) 0)           
	      (return-from score-diagonal-down (expt exp-base count))
	      (if (/= (aref board (+ column count) (- row (1+ count))) empty)  
		  (return-from score-diagonal-down (expt exp-base count))))))
  (if ( and (>= (- column 1) 0) (< (+ row 1) (array-dimension board 1)) ) ;;top left
      (if (= (aref board (- column 1) (+ row 1)) empty)  
	  (if (/= (aref board (- column 1) row) empty)  
	      (return-from score-diagonal-down (expt exp-base count)))))
  0
)

;;compute the score of a connected row (diagonal/), check if it's a valid row
(defun score-diagonal-up (board column row count exp-base)
  (if ( and (>= (- column 1) 0) (>= (- row 1) 0) )  ;;bottom left
      (if (= (aref board (1- column) (1- row)) empty)
	  (if (= (- row 1) 0)           
	      (return-from score-diagonal-up (expt exp-base count))
	      (if (/= (aref board (1- column) (- row 2)) empty) 
		  (return-from score-diagonal-up (expt exp-base count))))))
  (if ( and (< (+ column count) (array-dimension board 0)) (< (+ row count) (array-dimension board 1)) ) ;;top right
      (if (= (aref board (+ column count) (+ row count)) empty)  
	  (if (/= (aref board (+ column count) (+ row (1- count))) empty) 
	      (return-from score-diagonal-up (expt exp-base count)))))
  0
)

;;compute the score of a connected row (vertical), check if it's a valid row
(defun score-vertical (board column row count exp-base)
  (if (< (+ row 1) (array-dimension board 1)) 
      (if (= empty (aref board column (+ row 1)))
	  (return-from score-vertical (expt exp-base count))))
  0
)


;;compute the score of a connected row (horizontal), check if it's a valid row
(defun score-horizontal (board column row count exp-base)
  (if (>= (- column count) 0) ;;check left
      (if (= empty (aref board (- column count) row))
	  (if (= row 0)
	      (return-from score-horizontal (expt exp-base count))
	      (if (/= (aref board (- column count) (- row 1)) empty)
		  (return-from score-horizontal (expt exp-base count))))))
  (if (< (+ column 1) (array-dimension board 0)) ;;check right
      (if (= empty (aref board (+ column 1) row))
	  (if (= row 0)
	      (return-from score-horizontal (expt exp-base count))
	      (if (/= (aref board (+ column 1) (- row 1)) empty)
		  (return-from score-horizontal (expt exp-base count))))))
  0
)


;; I've decided to make this function available to you

(defun make-computer-move (game depth)
  "Makes a move automatically by trying alpha-beta on all possible moves and then picking
the one which had the highest value for max., and making that move and returning the new game."

  (let* ((max (turn game)))
    (max-element (all-moves game)
		 (lambda (g)
		   (alpha-beta g 0 depth 
			       (lambda (gm) (= (turn gm) max)) ;; is-maxs-turn-p
			       (lambda (gm) (all-moves gm)) ;; expand
			       (lambda (gm) (game-over gm)) ;; terminal-p
			       #'evaluate ;; evaluate
			       min-wins
			       max-wins)))))


;; go back to cl-user
(in-package :cl-user)
