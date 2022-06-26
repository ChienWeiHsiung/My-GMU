(defparameter *actions* '(:forward :backward))
(defparameter *states* '(0 1 2 3 4))
(defparameter *sensors* '(:even :odd))

(defun states () *states*) 

;;; A description of our action model

(defparameter *action-model*
        #3A((  ;; forward
                (.5 .5 0 0 0)
                (0 .5 .5 0 0)
                (0 0 .5 .5 0)
                (0 0 0 .5 .5)
                (.5 0 0 0 .5))

                ;; backward
                ((.5 0 0 0 .5)
                (.5 .5 0 0 0)
                (0 .5 .5 0 0)
                (0 0 .5 .5 0)
                (0 0 0 .5 .5)))
    "A table showing P(new | old, action) where action is either 
forward or backward, old is the row value, and new is the column 
value.  This basically models the notion that, in a toroidal 
one-dimensional world, if you move forward (or backward), you
have a 1/2 probability of accidentally staying put."
        ;;; note that the values in each row must sum to 1
)

(defun action-probability (new-state old-state action)
        "Returns the probability that, given an action and 
old state, the new state will be achieved"
        (aref *action-model* 
                (position action *actions*)
                (position old-state *states*)
                (position new-state *states*)))



;;;; A description of our sensor model


(defparameter *sensor-model*
        #2A(    (0.95 0.05)
                (0.05 0.95)
                (0.95 0.05)
                (0.05 0.95)
                (0.95 0.05))
    "A table showing P(sensor | state), where state is the row 
value, and sensor is the column value.  This basically models 
the notion that in a room of index i (0 to 4), your sensor 3/4 
of the time will give you 0 if i is even and 1 if i is odd.  
I dunno, maybe the rooms are light and dark or something.  :-)"
        ;;; note that the values in each row must sum to 1
)

(defun sensor-probability (sensor state)
        "Returns the probability that, given a state, that the
sensor will be the given value"
        (aref *sensor-model* 
                (position state *states*)
                (position sensor *sensors*)))






;;;;;; Some utility functions for normalizing tables, 
;;;;;; creating particles, and counting
;;;;;; particles at the end.

(defun normalize (lis)
        "Normalizes a table"
        (let ((sum (+ 0.0 (apply #'+ lis)))) ;; force to float
                (mapcar (lambda (elt) (/ elt sum)) lis)))

(defun random-elt (seq)
  "Returns a random element from a sequence"
  (elt seq (random (length seq))))

(defmacro gather (times &rest body)
  "Format:  (gather num-times -body-)
Calls the -body- code num-times times.  Each time the value of the last expression
in -body- is appended to a list.  The full list is then returned."
  (let ((x (gensym)) (y (gensym)))
    `(let (,y) (dotimes (,x ,times) (push (progn ,@body) ,y)) (nreverse ,y))))

(defun counts (distribution)
  "Counts the number of times a state particle appears in the distribution"
  (mapcar (lambda (state) (count state distribution)) *states*))





;;;;; Table-based Bayes Filter.  The function BAYES-FILTER takes a collection of
;;;;; previous beliefs about what state we're in and returns a new collection.
;;;;; Given our simple model above, these beliefs will take the form '(p0 p1 p2 p3 p4),
;;;;; which is just a table of probabilities, one per state.

;;;;;;; 1. Implement the Bayes and Particle Filters.

;; 1. IMPLEMENTED FUNCTION 1
(defun action-probabilities (new-state action)
	   "Given a new state and an action, returns a list of probabilities, one
per old state, of the probability of transitioning to the new state from
that old state given the action"
	   (mapcar (lambda (state) (action-probability new-state state action)) (states)))

;; 1. IMPLEMENTED FUNCTION 2
(defun sensor-probabilities (sensor)
	   "Given a sensor value, returns a list of probabilities, one
per state, of the probability of receiving that sensor value given the state"
	   (mapcar (lambda (state) (sensor-probability sensor state)) (states)))

;; 1. IMPLEMENTED FUNCTION 3
(defun bayes-filter (action sensor previous-beliefs)
           "Given a previous belief table, an action, and a sensor 
result, returns a new belief table about what our new states 
might possibly be.  Belief tables are simply lists of the form 
'(p1 p2 p3 p4 p5 ...) where p1 is the probability for the first 
state, p2 is the probability for the second state, and so on."
	   (let ((sensor-probs (sensor-probabilities sensor)) new-beliefs)
	     (dolist (state (states))
	       (let ((action-probs (action-probabilities state action)) second-part) ;;second-part is the probability sigma[P(X.|..)*P(x.|..)]
		 (setf second-part (reduce #'+ (mapcar #'* action-probs previous-beliefs))) ;;sum up action-probs(transition-model)*previous-beliefs(bel(xt-1) )
		 (push (* (nth (position state (states)) sensor-probs) second-part) new-beliefs)))
	     (normalize (reverse new-beliefs))))




;;;;; The particle filter.  The function PARTICLE-FILTER is similar to BAYES-FILTER
;;;;; except that its collections of beliefs take the form of BAGS of STATES.  The same
;;;;; state may repeatedly appear many times in this bag.  The number of times a state is
;;;;; is in the bag basically approximates the probability that we believe we're likely in
;;;;; that state.


;; I use this function to sample a random index from a distribution
;; of the form '(0.1 0.4 0.3 0.2)  for example.  In this example, the
;; returned value would be one of 0, 1, 2, or 3.  You can do this in O(n) if you
;; want, it's okay to be inefficient here.

;; 1. IMPLEMENTED FUNCTION 4
(defun sample-from-distribution (distribution)
  "Given a possibly non-normalized distribution, in the form of a list of probabilities,
selects from the distribution randomly and returns the index of the selected element."
  (let (index)
    (dotimes (i (length distribution)) ;;collect index of probabilities that > 0 in the distribution
      (if (> (nth i distribution) 0)
	  (push i index)))
  (nth (random (length index)) index))) ;; this is indirectly choosing a random index with non-zero probability

;; ALTERNATE APPROACH, probably lesser time complexity
;; (defun sample-from-distribution (distribution)
;; 	   "Given a possibly non-normalized distribution, in the form of a list of probabilities,
;; selects from the distribution randomly and returns the index of the selected element."
;; 	   (let (random-index)
;; 	     (loop
;; 	       (setf random-index (random (length distribution)))
;; 	       (if (> (nth random-index distribution) 0)
;; 		   (return random-index)))))


;; I use this function to convert a distribution of the form '((x1 w1) (x2 w2) ...)
;; into one like this: '(x1 x1 x1 x2 x2 x3 x4 x4 x4 x5 ...), basically randomly grabbing
;; x's from the previous distribution, proportionally to their weights, and sticking them
;; in the new distribution.  Note that the weights don't have to sum to 1 -- they're
;; weights, not probabilities.

;; 1. IMPLEMENTED FUNCTION 5
(defun resample-distribution (samples-and-weights &key (sample #'first) (weight #'second))
	   "Given a distribution (a list) of M elements, samples M times from that distribution and
forms a new distribution of elements.  Uses a Low-variance 'Stochastic Universal Sampling'
 (wikipedia for it) sampler, which is what the book uses; 
but a plain-old roulette wheel sampler or something else could have
been used just as well.  The function -sample- provides the probability for an element in the
list.  The function -weight- provides the resulting element that should be returned if 
selected.  By default these are #'first and #'second, presuming that the provided distribution is
of the form ((sample1 weight1) (sample2 weight2) ...)."
	   (let ((population (reduce #'append (mapcar (lambda (x)
							(make-list (floor (/ (funcall weight x) 0.05)) :initial-element (funcall sample x))) ;; dividing by 0.05 as the weights are fractions
						      samples-and-weights)))) ;; population is created by multiplying each sample by its weight
	     (let* ((F (length population)) (N (length samples-and-weights)) (P (floor (/ F N))) (Start (random P)) Pointers)
	       (dotimes (i N)
		 (push (+ start (* i P)) Pointers)) ;; Start + i*P
	       (setf Pointers (reverse Pointers))
	       (mapcar (lambda (x) (nth x population)) Pointers))))



;; Here I grab a random new state selected from the distribution of old ones.
;; note that I'm grabbing from a table -- that doesn't have to be the case,
;; I'm doing it to be compatable with my bayes filter examples.  In fact, you
;; don't have to have a "distribution" at all -- you can just create a function
;; which picks a random new state given the old state and action.  Often these
;; functions are much easier to write than generating a whole probability distribution.
;; And they allow arbitrarily complex continuous distributions to boot.  But here
;; we're going with the simple backward-compatable code...

;; 1. IMPLEMENTED FUNCTION 6
(defun select-from-action-probabilities (old-state action)
	   "Given an old-state and an action, selects a new-state at random and returns it
given the probability distribution P(new-state | old-state, action)."
	   (sample-from-distribution (mapcar (lambda (state) (action-probability state old-state action)) (states))))


;; Now we just do the belief update.  Note that beliefs now are different than they
;; were in the past: they're particles, each one representing a state.  Also note that
;; just as you could have sritten select-from-action-probabilities above without even
;; *having* a distribution, you can do the same for sensor-probability.  That's a major
;; strength of the particle filter.  It makes for easier representations of your
;; "distributions".

;; 1. IMPLEMENTED FUNCTION 7
(defun particle-filter (action sensor previous-beliefs)
  "Given a previous belief table, an action, and a sensor 
result, returns a  new belief table about what our new states 
might possibly be.  Belief tables are lists of particles.  Each
particle is simply a state."
  (let (first-sampling)
    (setf first-sampling (mapcar (lambda (x) (select-from-action-probabilities x action)) previous-beliefs)) ;; Randomly sample xt from the transition model
    (resample-distribution (mapcar (lambda (x) (list x (sensor-probability sensor x))) first-sampling)))) ;; Select an xt from Xt_bar, with replacement, using SUS








;;;;;;; 2. Demonstrate that the filters produce the same results.

;; EXAMPLE-1

;;; with the bayes filter
(defun example-1-bayes ()
  (let ((b (normalize '(1 1 1 1 1))))
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :forward :even b)) 
    (setf b (bayes-filter :forward :odd b)) 
    (setf b (bayes-filter :forward :even b)) 
    (setf b (bayes-filter :forward :even b)) 
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :forward :even b))
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :odd b)) 
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :odd b)) 
    (setf b (bayes-filter :backward :even b)) 
    (setf b (bayes-filter :backward :odd b))
    (setf b (bayes-filter :backward :even b))
    (format t "Bayes Filter Results: ~a" b)))

;; (example-1-bayes)
;; OUTPUT:
;; Bayes Filter Results: (0.7326335 0.040256143 0.19678028 0.0089635225 0.021366648)



;;; with the particle filter
(defun example-1-particle ()
  (let ((b (gather 10000 (random-elt *states*))))
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :forward :even b)) 
    (setf b (particle-filter :forward :odd b)) 
    (setf b (particle-filter :forward :even b)) 
    (setf b (particle-filter :forward :even b)) 
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :forward :even b))
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :odd b)) 
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :odd b)) 
    (setf b (particle-filter :backward :even b)) 
    (setf b (particle-filter :backward :odd b))
    (setf b (particle-filter :backward :even b))
    (format t "Particle Filter Results: ~a" (normalize (counts b)))))

;; (example-1-particle)
;; OUTPUT:
;; Particle Filter Results: (0.7267 0.044 0.1993 0.0094 0.0206)


;; EXAMPLE-2

;;; with the bayes filter
(defun example-2-bayes ()
  (let ((b '(1 0 0 0 0)))
    (setf b (bayes-filter :forward :even b))
    (setf b (bayes-filter :forward :odd b))
    (setf b (bayes-filter :backward :even b))
    (setf b (bayes-filter :backward :even b))
    (setf b (bayes-filter :backward :odd b))
    (format t "Bayes Filter Results: ~a" b)))

;; (example-2-bayes)
;; OUTPUT:
;; Bayes Filter Results: (0.045372415 0.004325214 2.1597654e-4 0.8596945 0.09039186)

;;; with the particle filter
(defun example-2-particle ()
  (let ((b (gather 10000 0)))
    (setf b (particle-filter :forward :even b))
    (setf b (particle-filter :forward :odd b))
    (setf b (particle-filter :backward :even b))
    (setf b (particle-filter :backward :even b))
    (setf b (particle-filter :backward :odd b))
    (format t "Particle Filter Results: ~a" (normalize (counts b)))))

;; (example-2-particle)
;; OUTPUT:
;; Particle Filter Results: (0.0473 0.005 2.0e-4 0.8555 0.092)








;;;;;;; What happens when you change the number of particles the particle filter uses?

;; For example 2:
;;
;; 1,000 Particles:
;; Particle Filter Results: (0.046 0.0 0.0 0.884 0.07)
;; Evaluation took:
;;   0.184 seconds of real time
;;   0.187500 seconds of total run time (0.171875 user, 0.015625 system)
;;   [ Run times consist of 0.063 seconds GC time, and 0.125 seconds non-GC time. ]
;;   102.17% CPU
;;   315,434,857 processor cycles
;;   432,679,920 bytes consed

;; 10,000 Particles:
;; Particle Filter Results: (0.0461 0.005 3.0e-4 0.8616 0.087)
;; Evaluation took:
;;   17.924 seconds of real time
;;   17.890625 seconds of total run time (15.046875 user, 2.843750 system)
;;   [ Run times consist of 4.492 seconds GC time, and 13.399 seconds non-GC time. ]
;;   99.82% CPU
;;   30,293,042,646 processor cycles
;;   43,242,454,384 bytes consed

;; 100,000 Particles:
;; Particle Filter Results: (0.04496 0.00478 2.6e-4 0.85816 0.09184)
;; Evaluation took:
;;   727.435 seconds of real time
;;   726.234375 seconds of total run time (572.703125 user, 153.531250 system)
;;   [ Run times consist of 343.885 seconds GC time, and 382.350 seconds non-GC time. ]
;;   99.83% CPU
;;   1,229,051,000,254 processor cycles
;;   1,080,049,076,160 bytes consed

;; From these observations we can say that:
;; When the number of particles is too low, the result processes quickly but doesn't converge too well, which can be observed by comparing with the results from Bayes Fitler.
;; When the number of particles is too large, the result converges well, but takes very long time to run.








;;;;;;; 3. Implement a fun new problem domain for one or both of the filters.  If you wish to
;;;;;;;    do a problem domain for FORWARD function rather than one for a filter,
;;;;;;;    you merely need to specify a single action!

;;; New problem
;;
;; Consider a robot vacuum that cleans a house with 9 rooms in a 3x3 grid.
;; Every room is connect to every other adjacent room, i.e., the robot has 4 actions, namely up, left, down and right.

;;; House design:
;;
;; -------------
;; | 0 | 1 | 2 |
;; -------------
;; | 3 | 4 | 5 |
;; -------------
;; | 6 | 7 | 8 |
;; -------------
;;

(defparameter *actions* '(:up :left :down :right))
(defparameter *states* '(0 1 2 3 4 5 6 7 8))

;; Senses the room number based on a code on the wall
(defparameter *sensors* '(:even :odd))
(defparameter *sensor-model*
           #2A((0.95 0.05)
               (0.05 0.95)
               (0.95 0.05)
               (0.05 0.95)
               (0.95 0.05)
               (0.05 0.95)
               (0.95 0.05)
               (0.05 0.95)
               (0.95 0.05)))

;; The following action model depicts that when a robot needs to perform an action in a state, there's:
;; 1. 60% chance the robot performs the desired action
;; 2. 10% chance the robot performs the opposite action
;; 3. 10% chance the robot performs one of the other 2 actions
;; 4. 10% chance the robot performs the leftover action
;; 5. 10% chance the robot stays in the same room
;; If in any case the robot can't move in a specific direction, then the probability adds up to the robot staying in the same state.

;; For instance, if the robot is in '4' and the action is 'up' results in:
;; 1. 60% chance robot goes to 1
;; 2. 10% chance robot goes to 7
;; 3. 10% chance robot goes to 3
;; 4. 10% chance robot goes to 5
;; 5. 10% chance robot stays in 4

;; For instance, 'down' in state '8' results in
;; 1. Robot can't go down, so another 60% chance robot stays in 8
;; 2. 10% chance robot goes to 5
;; 3. 10% chance robot goes to 7
;; 4. Robot can't go in any other direction, so another 10% chance robot stays in 8
;; 5. 10% actual chance + 60% chance from (1) + 10% chance from (4) = 80% chance robot stays in 8
;;; In total, there's an 80% chance the robot stays in the 8

(defparameter *action-model*
	   #3A(( ;; up
		(0.8 0.1 0 0.1 0 0 0 0 0)
		(0.1 0.7 0.1 0 0.1 0 0 0 0)
		(0 0.1 0.8 0 0 0.1 0 0 0)
		(0.6 0 0 0.2 0.1 0 0.1 0 0)
		(0 0.6 0 0.1 0.1 0.1 0 0.1 0)
		(0 0 0.6 0 0.1 0.2 0 0 0.1)
		(0 0 0 0.6 0 0 0.3 0.1 0)
		(0 0 0 0 0.6 0 0.1 0.2 0.1)
		(0 0 0 0 0 0.6 0 0.1 0.3))
	       
	       ( ;; left
		(0.8 0.1 0 0.1 0 0 0 0 0)
		(0.6 0.2 0.1 0 0.1 0 0 0 0)
		(0 0.6 0.3 0 0 0.1 0 0 0)
		(0.1 0 0 0.7 0.1 0 0.1 0 0)
		(0 0.1 0 0.6 0.1 0.1 0 0.1 0)
		(0 0 0.1 0 0.6 0.2 0.1 0 0)
		(0 0 0 0.1 0 0 0.8 0.1 0)
		(0 0 0 0 0.1 0 0.6 0.2 0.1)
		(0 0 0 0 0 0.1 0 0.6 0.3))

	       ( ;; down
		(0.3 0.1 0 0.6 0 0 0 0 0)
		(0.1 0.2 0.1 0 0.6 0 0 0 0)
		(0 0.1 0.3 0 0 0.6 0 0 0)
		(0.1 0 0 0.2 0.1 0 0.6 0 0)
		(0 0.1 0 0.1 0.1 0.1 0 0.6 0)
		(0 0 0.1 0 0.1 0.2 0 0 0.6)
		(0 0 0 0.1 0 0 0.8 0.1 0)
		(0 0 0 0 0.1 0 0.1 0.7 0.1)
		(0 0 0 0 0 0.1 0 0.1 0.8))

	       ( ;; right
		(0.3 0.6 0 0.1 0 0 0 0 0)
		(0.1 0.2 0.6 0 0.1 0 0 0 0)
		(0 0.1 0.8 0 0 0.1 0 0 0)
		(0.1 0 0 0.2 0.6 0 0.1 0 0)
		(0 0.1 0 0.1 0.1 0.6 0 0.1 0)
		(0 0 0.1 0 0.1 0.7 0 0 0.1)
		(0 0 0 0.1 0 0 0.3 0.6 0)
		(0 0 0 0 0.1 0 0.1 0.2 0.6)
		(0 0 0 0 0 0.1 0 0.1 0.8))))








;;;;;;; 4. Explore different situations in your new problem domain.

;; House design for reference:
;;
;; -------------
;; | 0 | 1 | 2 |
;; -------------
;; | 3 | 4 | 5 |
;; -------------
;; | 6 | 7 | 8 |
;; -------------
;;

;; Bayes Filter Test-1
;; Flow: 0-1-4-3-6-7-6-3-4-1-0
(defun new-example-1-bayes ()
	   (let ((b (normalize '(1 1 1 1 1 1 1 1 1))))
	     (setf b (bayes-filter :right :odd b))
	     (setf b (bayes-filter :down :even b))
	     (setf b (bayes-filter :left :odd b))
	     (setf b (bayes-filter :down :even b))
	     (setf b (bayes-filter :right :odd b))
	     (setf b (bayes-filter :left :even b))
	     (setf b (bayes-filter :up :odd b))
	     (setf b (bayes-filter :right :even b))
	     (setf b (bayes-filter :up :odd b))
	     (setf b (bayes-filter :left :even b))
	     (format t "Bayes Filter Results: ~a~%" b)))
;; (new-example-1-bayes)
;; Bayes Filter Results: Bayes Filter Results: (0.4278178 0.007875033 0.10526086 0.008576821 0.2906099 0.003475232 0.13959984 0.001637676 0.01514686)
;;
;; OBSERVATION: The predicted state 0 is correct initial state.


;; Bayes Filter Test-2
;; Flow: 4-7-6-3-0-1-2-5-8-7-4
(defun new-example-2-bayes ()
	   (let ((b (normalize '(1 1 1 1 1 1 1 1 1))))
	     (setf b (bayes-filter :down :odd b))
	     (setf b (bayes-filter :left :even b))
	     (setf b (bayes-filter :up :odd b))
	     (setf b (bayes-filter :up :even b))
	     (setf b (bayes-filter :right :odd b))
	     (setf b (bayes-filter :right :even b))
	     (setf b (bayes-filter :down :odd b))
	     (setf b (bayes-filter :down :even b))
	     (setf b (bayes-filter :left :odd b))
	     (setf b (bayes-filter :up :even b))
	     (format t "Bayes Filter Results: ~a~%" b)))
;; (new-example-2-bayes)
;; Bayes Filter Results: (0.21280614 0.0063772476 0.118507534 0.0038505502 0.46032026 0.0022535315 0.10197977 0.0070865387 0.08681832)
;;
;; OBSERVATION: The predicted state 4 is correct initial state.


;; Bayes Filter Test-3
;; Flow: 3-6-7-8-5-2-1-0-3
(defun new-example-3-bayes ()
	   (let ((b (normalize '(1 1 1 1 1 1 1 1 1))))
	     (setf b (bayes-filter :down :even b))
	     (setf b (bayes-filter :right :odd b))
	     (setf b (bayes-filter :right :even b))
	     (setf b (bayes-filter :up :odd b))
	     (setf b (bayes-filter :up :even b))
	     (setf b (bayes-filter :left :odd b))
	     (setf b (bayes-filter :left :even b))	
	     (setf b (bayes-filter :down :odd b))
	     (format t "Bayes Filter Results: ~a~%" b)))
;; (new-example-3-bayes)
;; OUTPUT:
;; Bayes Filter Results: (0.011382493 0.122246 0.0023093761 0.4892507 0.0023419533 0.122821204 0.010943278 0.23739262 0.0013124427)
;;
;; OBSERVATION: The predicted state 3 is the correct initial state.