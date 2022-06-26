;;; This is the "extended" version of Project 1

#|
In this project you will produce three things:

1. A high-level evolutionary computation framework

2. The representation, breeding functions, and evaluations to do a simple
GA for boolean vectors and for floating-point vectors, including a few test
functions on each.

3. The representation, breeding functions, and evaluations to do GP for
two problems:
A. Symbolic Regression
B. Artificial Ant

The high-level EC system will work as follows:

- Simple generational evolution
- GA-style Tournament selection
- A simple breeding function
- some simple statistics functions

I have provided the approximate function templates I myself used to complete
the task; with permission you can use these or go your own way, but otherwise
please try not to deviate much from this template.

The project is due approximately two and a half weeks from now or so.  Please 
try to get it in on time.

WHAT YOU MUST PROVIDE:

1. Completed code which works and compiles.  As simple as possible would be nice.

2. A short report describing the code you wrote and some experimentation with it.
Some things to try:
   -- different problems (in the vector section)
   -- different settings of mutation and crossover parameters
   -- different population sizes or run lengths
Try to get a handle on how problem difficulty changes when you tweak various
parameters.  Can you get mutation and crossover parameters which seem optimal for
a given problem for example?  (Obviously bigger population sizes are more optimal
but that's kinda cheating).  Note that this is a STOCHASTIC problem so you'll need
to run a number of times and get the mean best result in order to say anything of
consequence.  It'd be nice if the report were in LaTeX and it'd be good practice for
you as well, but it's not necessary.  I do not accept reports in Word.  Only send
me a PDF.

Make sure your code is compiled.  In most cases (such as SBCL), your code will be
automatically compiled.  But there exist systems (like LispWorks ("lisp" on 
mason.gmu.edu)) where the default is to interpret the code, and so you must 
compile your file first and then load that.

Information on compiling code and doing compiler optimizations can be found in the
"Speed" chapter of Graham.
|#


;;; Useful Functions and Macros

(defmacro swap (elt1 elt2)
  "Swaps elt1 and elt2, using SETF.  Returns nil."
  (let ((temp (gensym)))
    `(let ((,temp ,elt1))
       (setf ,elt1 ,elt2)
       (setf ,elt2 ,temp)
       nil)))

(defmacro while (test &rest body)
	   "Repeatedly executes body as long as test returns true.  Then returns nil."
	   `(loop while ,test do (progn ,@body)))

(defun random-elt (sequence)
  "Returns a random element from sequence"
  (elt sequence (random (length sequence))))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
	   "Generates a list of size NUM, with each element created by (funcall FUNCTION).  If no-duplicates is t, then no duplicates are permitted (FUNCTION is repeatedly called until a unique new slot is created).  EQUALP is the default test used for duplicates."
	   (let (bag)
	     (while (< (length bag) num)
		    (let ((candidate (funcall function)))
		      (unless (and no-duplicates
				   (member candidate bag :test #'equalp))
			(push candidate bag))))
	     bag))







;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 





;;; TOURNAMENT SELECTION

(defparameter *tournament-size* 7)

(defun tournament-select-one (population fitnesses)
	   "Does one tournament selection and returns the selected individual."
	   (let ((x 2) next-index (best-index (random (length population))))
	     (while (<= x *tournament-size*)
		    (setf next-index (random (length population)))
		    (if (> (elt fitnesses next-index) (elt fitnesses best-index))
			(setf best-index next-index))
		    (incf x))
	     (elt population best-index)))



(defun tournament-selector (num population fitnesses)
	   "Does NUM tournament selections, and puts them all in a list, then returns the list"
	   (generate-list num (lambda () (tournament-select-one population fitnesses))))


(defun simple-printer (pop fitnesses)  ;; I'm nice and am providing this for you.  :-)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))


(defun evolve (generations pop-size &key setup creator selector modifier evaluator printer)
	   "Evolves for some number of GENERATIONS, creating a population of size POP-SIZE, using various functions"
	   (funcall setup)
	   (let ((population (generate-list pop-size creator)) best-index fitnesses selected-individuals ind1 ind2 q)
	     (dotimes (x generations (elt population best-index))
	       (setf fitnesses (mapcar evaluator population))
	       (dotimes (i (length population))
		 (if (or (null best-index) (> (elt fitnesses i) (elt fitnesses best-index)))
		     (setf best-index i)))
	       (setf q '())
	       (dotimes (x_q (/ pop-size 2))
		 (setf selected-individuals (funcall selector 2 population fitnesses))
		 (setf ind1 (first selected-individuals))
		 (setf ind2 (second selected-individuals))
		 (setf q (append q (funcall modifier ind1 ind2))))
	       (setf population q)
	       (funcall printer population fitnesses))))




(defun evolve-n (n evl func)
	   "Performs evolve n times and finds the best result in it"
	   (let (res best)
	     (dotimes (x n)
	       (setf res (funcall func))
	       (if (or (null best) (> (funcall evl res) (funcall evl best)))
		   (setf best res)))
	     (format t "~%~%After ~a runs:~%~%Best fitness: ~a~%~%Best Individual: ~a~%~%" n (funcall evl best) best)))



;;;;;; BOOLEAN VECTOR GENETIC ALGORTITHM






;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; boolean vectors Problem.  Assume that you are evolving a bit vector
;;; *boolean-vector-length* long.  

;;; The default test function is Max-Ones.
;;;; Max-ones is defined in section 11.2.1 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other boolean functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :max-ones
;;; :trap
;;; :leading-ones
;;; :leading-ones-blocks



(defparameter *boolean-vector-length* 100)
(defparameter *boolean-crossover-probability* 0.2)
(defparameter *boolean-mutation-probability* 0.01)

(defun max-ones-f (ind)
	   "Fitness is the total number of ones in the boolean vector."
	   (reduce #'+ ind))

(defun trap-f (ind)
	   "Fitness is the number of zeros in the vector. If there are no zeros, then the fitness is the 1 + length of ind"
	   (if (position 0 ind)
	       (count-if (lambda (x) (= x 0)) ind)
	       (1+ (length ind))))

(defun leading-ones-f (ind)
	   "Fitness is the total number of ones in the boolean vector, starting at the beginning, until a zero is encountered."
	   (position 0 ind))

(defun leading-ones-blocks-f (ind)
	   "Given a value b, fitness is the number of string of ones, each b long, until a zero is encountered."
	   (floor (/ (leading-ones-f ind) 3)))

(defparameter *boolean-problem* #'max-ones-f)

(defun boolean-vector-creator ()
	   "Creates a boolean-vector *boolean-vector-length* in size, filled with random Ts and NILs, or with random 1s and 0s, your option."
	   (generate-list *boolean-vector-length* (lambda () (if (random?) 1 0))))

(defun boolean-uniform-crossover (ind1 ind2)
	   "Performs uniform crossover on the two individuals, modifying them in place. *crossover-probability* is the probability that any given allele will crossover. The individuals are guaranteed to be the same length.  Returns NIL."
	   (dotimes (i (length ind1))
	     (if (random? *boolean-crossover-probability*) (swap (elt ind1 i) (elt ind2 i)))))

(defun mutation-boolean (ind)
	   (dotimes (i (length ind))
	     (if (random? *boolean-mutation-probability*)
		 (setf (elt ind i) (if (= (elt ind i) 0) 1 0)))))

(defun boolean-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation simply flips the bit of the allele."
	   (let ((ind1-copy (copy-seq ind1)) (ind2-copy (copy-seq ind2)))
	     (boolean-uniform-crossover ind1-copy ind2-copy)
	     (mutation-boolean ind1-copy)
	     (mutation-boolean ind2-copy)
	     (list ind1-copy ind2-copy)))

(defun boolean-vector-evaluator (ind1)
	   "Evaluates an individual, which must be a boolean-vector, and returns its fitness."
	   (funcall *boolean-problem* ind1))

(defun boolean-vector-sum-setup ()
	   "Does nothing.  Perhaps you might use this to set up various (ahem) global variables to define the problem being evaluated, I dunno."
	   (setf *boolean-vector-length* 100)
	   (setf *tournament-size* 7)
	   (setf *boolean-crossover-probability* 0.2)
	   (setf *boolean-mutation-probability* 0.01))


;;; an example way to fire up the GA.  It should easily discover
;;; a 100% correct solution.
#|
(evolve-n
	  50
	  #'boolean-vector-evaluator
	  (lambda ()
	    (evolve 50 100
 		    :setup #'boolean-vector-sum-setup
		    :creator #'boolean-vector-creator
		    :selector #'tournament-selector
		    :modifier #'boolean-vector-modifier
		    :evaluator #'boolean-vector-evaluator
		    :printer #'simple-printer)))

Detailed results in report

|#







;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM




;;; Creator, Modifier, Evaluator, and Setup functions for the
;;; GA Max-ONES Problem.  Assume that you are evolving a vector
;;; of floating-point numbers *float-vector-length* long.  


;;; The default test function is Rastrigin.
;;;; Rastrigin is defined in section 11.2.2 of "Essentials of Metaheuristics"
;;; by yours truly.  In that section are defined several other floating-point functions
;;; which you should also try and report on.  Some plausible ones might
;;; include:

;;; :rastrigin
;;; :rosenbrock
;;; :griewank
;;; :schwefel

;;; If you were really into this, you might also try testing on
;;; rotated problems -- some of the problems above are linearly
;;; separable and rotation makes them harder and more interesting.
;;; See the end of Section 11.2.2.

;;; I have defined the minimum and maximum gene values for rastrigin
;;; as [-5.12, 5.12].  Other problems have other traditional min/max
;;; values, consult Section 11.2.2.

(defun sum-f (ind)
	   "Performs the Sum objective function.  Assumes that ind is a list of floats"
	   (reduce #'+ ind))

(defun step-f (ind)
	   "Performs the Step objective function.  Assumes that ind is a list of floats"
	   (+ (* 6 (length ind))
	      (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
	   "Performs the Sphere objective function.  Assumes that ind is a list of floats"
	   (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
	   "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
	   (- (reduce #'+ (mapcar (lambda (x x1)
				    (+ (* (- 1 x) (- 1 x))
				       (* 100 (- x1 (* x x)) (- x1 (* x x)))))
				  ind (rest ind)))))

(defun rastrigin-f (ind)
	   "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
	   (- (+ (* 10 (length ind))
		 (reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
				     ind)))))

(defun schwefel-f (ind)
	   "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
	   (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
				  (mapcar (lambda (x) (* x 100)) ind)))))

(defun gaussian-random (mean variance)
	   "Generates a random number under a gaussian distribution with the given mean and variance (using the Box-Muller-Marsaglia method)"
	   (let ((x 0) y (w 0))
	     (while (not (and (< 0 w) (< w 1)))
		 (setf x (- (random 2.0) 1.0))
	       (setf y (- (random 2.0) 1.0))
	       (setf w (+ (* x x) (* y y))))
	     (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))

(defparameter *float-vector-length* 100)
(defparameter *float-min* -5.12)  ;; these will change based on the problem
(defparameter *float-max* 5.12)   ;; likewise
(defparameter *float-crossover-probability* 0.2)
(defparameter *float-mutation-probability* 0.1)   ;; I just made up this number
(defparameter *float-mutation-variance* 0.01)     ;; I just made up this number
(defparameter *float-problem* #'rastrigin-f)

(defun float-vector-creator ()
	   "Creates a floating-point-vector *float-vector-length* in size, filled with UNIFORM random numbers in the range appropriate to the given problem"
	   (generate-list *float-vector-length* (lambda () (+ *float-min* (random (- *float-max* *float-min*))))))

(defun float-uniform-crossover (ind1 ind2)
	   "Performs uniform crossover on the two individuals, modifying them in place. *crossover-probability* is the probability that any given allele will crossover. The individuals are guaranteed to be the same length.  Returns NIL."
	   (dotimes (i (length ind1))
	     (if (random? *float-crossover-probability*) (swap (elt ind1 i) (elt ind2 i)))))

(defun gaussian-convolution (ind)
	   "Performs gaussian convolution mutation on the individual, modifying it in place. Returns NIL."
	   (dotimes (i (length ind))
	     (if (random? *float-mutation-probability*)
		 (let ((n (gaussian-random 0 *float-mutation-variance*)))
		   (while (not (and (> (+ n (elt ind i)) *float-min*) (< (+ n (elt ind i)) *float-max*)))
		       (setf n (gaussian-random 0 *float-mutation-variance*)))
		   (setf (elt ind i) (+ n (elt ind i)))))))

(defun float-vector-modifier (ind1 ind2)
	   "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover, then mutates the children.  *crossover-probability* is the probability that any given allele will crossover.  *mutation-probability* is the probability that any given allele in a child will mutate.  Mutation does gaussian convolution on the allele."
	   (let ((ind1-copy (copy-seq ind1)) (ind2-copy (copy-seq ind2)))
	     (float-uniform-crossover ind1-copy ind2-copy)
	     (gaussian-convolution ind1-copy)
	     (gaussian-convolution ind2-copy)
	     (list ind1-copy ind2-copy)))

(defun float-vector-sum-evaluator (ind1)
	   "Evaluates an individual, which must be a floating point vector, and returns its fitness."
	   (funcall *float-problem* ind1))

(defun float-vector-sum-setup ()
	   "Does nothing.  Perhaps you might use this function to set (ahem) various global variables which define the problem being evaluated and the floating-point ranges involved, etc. I dunno."
	   (setf *tournament-size* 7)
	   (setf *float-vector-length* 20)
	   (setf *float-min* -5.12)
	   (setf *float-max* 5.12)
	   (setf *float-crossover-probability* 0.1)
	   (setf *float-mutation-probability* 0.1)
	   (setf *float-mutation-variance* 0.02))

;;; an example way to fire up the GA.  

#|
(evolve-n
	  50
	  #'float-vector-sum-evaluator
	  (lambda ()
	    (evolve 50 100
 		    :setup #'float-vector-sum-setup
		    :creator #'float-vector-creator
		    :selector #'tournament-selector
		    :modifier #'float-vector-modifier
		    :evaluator #'float-vector-sum-evaluator
		    :printer #'simple-printer)))

Detailed results in report

|#










;;;; GP TREE CREATION CODE

;;; GP's tree individuals are considerably more complex to modify than
;;; simple vectors.  Get the GA system working right before tackling
;;; this part, trust me.



;; set up in the gp setup function -- for example, see
;; the code for gp-symbolic-regression-setup
(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)



;;; important hint: to use SETF to change the position of an element in a list
;;; or a tree, you need to pass into SETF an expression starting with the
;;; parent.  For example (setf (car parent) val) .... thus you HAVE to have
;;; access to the parent, not just the child.  This means that to store a
;;; "position", you have to store away the parent and the arg position of
;;; child, not the child itself.

(defun make-queue ()
  "Makes a random-queue"
  (make-array '(0) :adjustable t :fill-pointer t))

(defun enqueue (elt queue)
  "Enqueues an element in the random-queue"
  (progn (vector-push-extend elt queue) queue))

(defun queue-empty-p (queue)
  "Returns t if random-queue is empty"
  (= (length queue) 0))

(defun random-dequeue (queue)
  "Picks a random element in queue and removes and returns it.
Error generated if the queue is empty."
  (let ((index (random (length queue))))
    (swap (elt queue index) (elt queue (1- (length queue))))
    (vector-pop queue)))

(defun ptc2 (size)
	   "If size=1, just returns a random terminal.  Else builds and
returns a tree by repeatedly extending the tree horizon with
nonterminals until the total number of nonterminals in the tree,
plus the number of unfilled slots in the horizon, is >= size.
Then fills the remaining slots in the horizon with terminals.
Terminals like X should be added to the tree
in function form (X) rather than just X."

	   ;;random-node : temporarily store a randomly-chosen node from two sets
	   ;;queue-item : temporarily store a randomly-chosen item from queue
	   ;;temp : Just temporarily variable
	   (let ((queue) (root) (count) (random-node) (queue-item) (temp))
	     (when (= size 1) ;;size = 1
	       (return-from ptc2 (list (random-elt *terminal-set*))))
	     (setf queue (make-queue)) ;; q <- make queue
	     ;; root <- random nonterminal
	     ;; transform (% 2) to (% (1) (1)) for making slot clear
	     (setf random-node (copy-list (random-elt *nonterminal-set*))) 
	     (setf root (list (car random-node))) ;(%)
	     (dotimes (i (elt random-node 1))     ;(% (1) (1))
	       (setf root (append root '((0)) )))
	     (setf count 1) ;;count <- 1
	     ;;enqueue into q each child argument slot of root
	     ;;In the form of ((% (1) (1)) 1) ((% (1) (1)) 2) <= node & index of slot
	     (dotimes (i (elt random-node 1))
	       (enqueue (list root (1+ i)) queue))
	     ;;Loop until count + size_of_q >= size
	     (loop
	       ;;remove a random argument slot s from q
	      (setf queue-item (random-dequeue queue))
	      ;;a <- random nonterminal
	      ;;transformation. the same as above
	      (setf random-node (copy-list (random-elt *nonterminal-set*)))
	      (setf temp (list (car random-node)))
	      (dotimes (i (elt random-node 1))
		(setf temp (append temp '(0))))
	      ;;count <- count + 1
	      (setf count (1+ count))
	      (setf temp (copy-list temp))
	      ;;fill the slot s with a
	      (setf (elt (car queue-item) (car (cdr queue-item))) temp)
	      ;;enqueue into q each child argument slot of a
	      (dotimes (i (elt random-node 1))
		(enqueue (list temp (1+ i)) queue))
	      ;;exit condition
	      (if (>= (+ count (length queue)) size)
		  (return-from nil)))
	     ;;Loop until size_of_q = 0
	     (while (> (length queue) 0)
		    ;;remove a random argument slot s from q
		    (setf queue-item (random-dequeue queue))
		    ;;a <- random terminal
		    ;;fill the slot s with a
		    (setf (elt (car queue-item) (car (cdr queue-item))) (list (random-elt *terminal-set*))))
	     (copy-tree root)))


(defparameter *size-limit* 20)

(defun gp-creator ()
	   "Picks a random number from 1 to 20, then uses ptc2 to create a tree of that size"
	   (ptc2 (1+ (random *size-limit*))))


;;; GP TREE MODIFICATION CODE

(defun num-nodes (tree)
	   "Returns the number of nodes in tree, including the root"
	   (if tree
	       (+ (if (atom (car tree))
		      1
		      (num-nodes (car tree)))
		  (num-nodes (cdr tree)))
	       0))


(defun nth-subtree-parent (tree n)
	   "Given a tree, finds the nth node by depth-first search though
the tree, not including the root node of the tree (0-indexed). If the
nth node is NODE, let the parent node of NODE is PARENT,
and NODE is the ith child of PARENT (starting with 0),
then return a list of the form (PARENT i).  For example, in
the tree (a (b c d) (f (g (h) i) j)), let's say that (g (h) i)
is the chosen node.  Then we return ((f (g (h) i) j) 0).

If n is bigger than the number of nodes in the tree
 (not including the root), then we return n - nodes_in_tree
 (except for root)."
	   (let* ((parent tree) (child (cdr parent)) (stack '()) (count 0) (pop-node))
	     (if (>= n (1- (num-nodes tree))) ;;if n exceeds the size of tree
		 (return-from nth-subtree-parent (- n (1- (num-nodes tree)))))
	     (dotimes (i (length child)) ;;First layer ( parent is root, child is childs of the root )
	       (setf stack (append stack (list (list parent i ))))) ;;use stack to store in the form of (Parent i)
	     (while stack
		    (setf pop-node (pop stack)) ;;pop
		    (if (= count n) ;;return nth node, else increment count
			(return-from nth-subtree-parent pop-node))
		    (setf count (1+ count))
		    (setf parent (elt (cdr (car pop-node)) (car (cdr pop-node))))
		    (when (listp parent)
		      (setf child (cdr parent))
		      (dotimes (i (length child))
			(push (list parent (- (length child) i 1)) stack))))))


(defparameter *mutation-size-limit* 10)

(defun gp-modifier (ind1 ind2)
	   "Flips a coin.  If it's heads, then ind1 and ind2 are
crossed over using subtree crossover.  If it's tails, then
ind1 and ind2 are each mutated using subtree mutation, where
the size of the newly-generated subtrees is pickedc at random
from 1 to 10 inclusive.  Doesn't damage ind1 or ind2.  Returns
the two modified versions as a list."
	   ;; tree1, tree2 : copy of ind1, ind2
	   ;; subtree1, subtree2 : subtree picked from "nth-subtree-parent" function based on random-number (Reminder : not including root)
	   ;; random-number1, random-number2 : random number for tree1, tree2
	   (let ((tree1 (copy-tree ind1)) (tree2 (copy-tree ind2)) (subtree-1) (subtree-2) (random-number1) (random-number2))
	     (if (>= (random 1.0) 0.5)
		 (progn ;;subtree crossover
		   (setf random-number1 (random (num-nodes tree1))) ;;pick a random number based on the size of tree 
		   (setf random-number2 (random (num-nodes tree2)))
		   (when (and (= random-number1 0) (= random-number2 0) );;both pick the root
		     (swap tree1 tree2))
		   (when (and (= random-number1 0) (/= random-number2 0) );;tree1 picks the root, tree2 not
		     (setf subtree-2 (nth-subtree-parent tree2 (1- random-number2))) ;;random-number has to minus 1 because function "nth-subtree-parent" doesn't include root.
		     (swap tree1 (elt (cdr (car subtree-2)) (car (cdr subtree-2)))))
		   (when (and (/= random-number1 0) (= random-number2 0) );;tree2 picks the root, tree1 not
		     (setf subtree-1 (nth-subtree-parent tree1 (1- random-number1)))
		     (swap (elt (cdr (car subtree-1)) (car (cdr subtree-1))) tree2))
		   (when (and (/= random-number1 0) (/= random-number2 0) );;neither pick the root
		     (setf subtree-1 (nth-subtree-parent tree1 (1- random-number1)))
		     (setf subtree-2 (nth-subtree-parent tree2 (1- random-number2)))
		     (swap (elt (cdr (car subtree-1)) (car (cdr subtree-1))) (elt (cdr (car subtree-2)) (car (cdr subtree-2)))))
		   ;;After crossover, If tree size exceed *size-limit*, create a random tree
		   (if (> (num-nodes tree1) *size-limit*)
		       (setf tree1 (ptc2 (1+ (random *size-limit*) ))))
		   (if (> (num-nodes tree2) *size-limit*)
		       (setf tree2 (ptc2 (1+ (random *size-limit*) ))))
		   (list tree1 tree2));;progn : if true
		 (progn ;;subtree mutation
		   (setf random-number1 (random (num-nodes tree1))) ;;pick a random number based on the size of tree 
		   (setf random-number2 (random (num-nodes tree2)))
		   (if (= random-number1 0) ;;if pick the root
		       (setf tree1 (ptc2 (1+ (random *mutation-size-limit*))))
		       (progn ;;else, pick a subtree
			 (setf subtree-1 (nth-subtree-parent tree1 (1- random-number1)))
			 (setf (elt (cdr (car subtree-1)) (car (cdr subtree-1)))  (ptc2 (1+ (random *mutation-size-limit*))))))
		   (if (= random-number2 0) ;;if pick the root
		       (setf tree2 (ptc2 (1+ (random *mutation-size-limit*))))
		       (progn ;;else, pick a subtree
			 (setf subtree-2 (nth-subtree-parent tree2 (1- random-number2)))
			 (setf (elt (cdr (car subtree-2)) (car (cdr subtree-2)))  (ptc2 (1+ (random *mutation-size-limit*))))))
		   ;;After crossover, If tree size exceed *size-limit*, create a random tree
		   (if (> (num-nodes tree1) *size-limit*)
		       (setf tree1 (ptc2 (1+ (random *size-limit*) ))))
		   (if (> (num-nodes tree2) *size-limit*)
		       (setf tree2 (ptc2 (1+ (random *size-limit*) ))))
		   (list tree1 tree2)))))





;;; SYMBOLIC REGRESSION
;;; This problem domain is similar, more or less, to the GP example in
;;; the lecture notes.  Your goal is to make a symbolic expression which
;;; represents a mathematical function consisting of SIN COS, EXP,
;;; +, -, *, and % (a version of / which doesn't give an error on
;;; divide-by-zero).  And also the function X, which returns a current
;;; value for the X variable.
;;;
;;; In symbolic regression, we generate 20 (x,y) pairs produced at
;;; random which fit the expression y = x^4 + x^3 + x^2 + x.  You can
;;; make up another function is you like, but that's the one we're going
;;; with here.  Using just these data pairs, the GP system will attempt
;;; to ascertain the function.  It will generate possible expressions
;;; as its individuals, and the fitness of an expression is how closely,
;;; for each X value, it gives the correct corresponding Y value.
;;;
;;; This is called SYMBOLIC regression because we're actually learning
;;; the mathematical expression itself, including transcendental functions
;;; like SIN and COS and E^.  As opposed to statistical linear or
;;; quadratic curve-fitting regressions which just try to learn the
;;; linear or quadratic parameters etc.
;;;
;;; An example 100% optimal solution:
;;;
;;; (+ (* (x) (* (+ (x) (* (x) (x))) (x))) (* (+ (x) (cos (- (x) (x)))) (x)))



;;; GP SYMBOLIC REGRESSION SETUP
;;; (I provide this for you)

(defparameter *num-vals* 20)
(defparameter *vals* nil) ;; gets set in gp-setup

(defun gp-symbolic-regression-setup ()
  "Defines the function sets, and sets up vals"

  (setq *nonterminal-set* '((+ 2) (- 2) (* 2) (% 2) (sin 1) (cos 1) (exp 1)))
  (setq *terminal-set* '(x))

  (setq *vals* nil)
  (dotimes (v *num-vals*)
    (push (1- (random 2.0)) *vals*)))

(defun poly-to-learn (x) (+ (* x x x x) (* x x x) (* x x) x))

;; define the function set
(defparameter *x* nil) ;; to be set in gp-evaluator
(defun x () *x*)
(defun % (x y) (if (= y 0) 0 (/ x y)))  ;; "protected division"
;;; the rest of the functions are standard Lisp functions




;;; GP SYMBOLIC REGRESSION EVALUATION

(defun gp-symbolic-regression-evaluator (ind)
	   "Evaluates an individual by setting *x* to each of the
elements in *vals* in turn, then running the individual and
get the output minus (poly-to-learn *x*).  Take the
absolute value of the this difference.  The sum of all such
absolute values over all *vals* is the 'raw-fitness' Z.  From
this we compute the individual's fitness as 1 / (1 + z) -- thus
large values of Z are low fitness.  Return the final
individual's fitness.  During evaluation, the expressions
evaluated may overflow or underflow, or produce NaN.  Handle all
such math errors by
returning most-positive-fixnum as the output of that expression."
	   (let ((fitness 0.0))
	     (dotimes (i (length *vals*))
	       (handler-case
		   (progn
		     (setf *x* (elt *vals* i))
		     (setf fitness (+ fitness (abs (- (poly-to-learn *x*) (eval ind))))))
		 (error (condition)
		   (format t "~%Warning, ~a" condition) most-positive-fixnum)))
	     (setf fitness (/ 1 (+ 1 fitness)))))

;;; Example run
#|
(evolve 50 500
 		 :setup #'gp-symbolic-regression-setup
		 :creator #'gp-creator
		 :selector #'tournament-selector
		 :modifier #'gp-modifier
		 :evaluator #'gp-symbolic-regression-evaluator
		 :printer #'simple-printer)
|#

#|

Best Fitness: 1.0
Best Individual:(+ (X) (* (- (+ (X) (X)) (X)) (+ (X) (* (X) (+ (* (X) (X)) (X))))))
(+ (X) (* (- (+ (X) (X)) (X)) (+ (X) (* (X) (+ (* (X) (X)) (X))))))

(x = 0.9697573)

|#



;;; GP ARTIFICIAL ANT CODE
;;; for this part you'll need to implement both the evaluator AND
;;; make up the function that form the function set.

;;; In the artificial ant problem domain, you'll be evolving an s-expression
;;; which moves an ant around a toroidal map shown below.  The ant starts out
;;; at (0,0), facing east (to the right).  The functions in
;;; the expression are:
;;; (if-food-ahead --then-- --else--)   If food is directly ahead of the ant,
;;;                                     then evaluate the THEN expression, else
;;;                                     evaluate the ELSE expression
;;; (progn2 --item1-- --item2--)        Evaluate item1, then evaluate item 2
;;; (progn3 --item1-- --item2-- --item3--)  Evaluate item1, then item2, then item3
;;; (move)                              Move forward one unit
;;;                                     If you pass over a food pellet, it is eaten
;;;                                     and removed from the map.
;;; (left)                              Turn left
;;; (right)                             Turn right
;;;
;;;
;;; the way a tree is evaluated is as follows.  The whole tree is evaluated,
;;; and the functions move the ant about.  If the ant has not made a total of
;;; 600 MOVE, LEFT, and RIGHT operations yet, then the tree is evaluated AGAIN
;;; moving the ant some more from its current position.  This goes on until
;;; 600 operations have been completed, at which time the ant will not move
;;; any more.  If 600 operations are completed in the middle of the evaluation
;;; of a tree, the simplest approach is to have the MOVE command simply
;;; "stop working" so the ant doesn't gobble up any more pellets accidentally.

;;; The fitness of the artificial ant is the number of pellets it ate in the
;;; 600-operation period.  Maps are reset between evaluations of different
;;; individuals of course.

;;; Here's an optimal ant program (there are many such):
;;  (progn3 (if-food-ahead (move) (progn2 (left) (progn2 (left)
;;             (if-food-ahead (move) (right))))) (move) (right))

;;; This is a hard problem for GP and you may need to run many times before you
;;; get a 100% perfect answer.

;;; Note that in my thesis it says 400 moves and not 600.  We're going with
;;; 600 here.  It's easier.



;;; our ant's food trail map
(defparameter *map-strs* '(
".###............................"
"...#............................"
"...#.....................###...."
"...#....................#....#.."
"...#....................#....#.."
"...####.#####........##........."
"............#................#.."
"............#.......#..........."
"............#.......#..........."
"............#.......#........#.."
"....................#..........."
"............#..................."
"............#................#.."
"............#.......#..........."
"............#.......#.....###..."
".................#.....#........"
"................................"
"............#..................."
"............#...#.......#......."
"............#...#..........#...."
"............#...#..............."
"............#...#..............."
"............#.............#....."
"............#..........#........"
"...##..#####....#..............."
".#..............#..............."
".#..............#..............."
".#......#######................."
".#.....#........................"
".......#........................"
"..####.........................."
"................................"))

(defparameter *map-height* 32)
(defparameter *map-width* 32)

;; The four directions.  For relative direction, you might
;; assume that the ant always PERCEIVES things as if it were
;; facing north.
(defconstant *n* 0)
(defconstant *e* 1)
(defconstant *s* 2)
(defconstant *w* 3)

;;; some useful functions for you

(defun make-map (lis)
  "Makes a map out of a string-list such as *MAP-STRS*"
  (let ((map (make-array (list (length (first lis)) (length lis)))))
    (dotimes (y (length lis) map)
      (dotimes (x (length (elt lis y)))
	(setf (aref map x y)
	      (cond ((equalp #\# (elt (elt lis y) x)) nil)
		    (t t)))))))

(defun direction-to-arrow (dir)
  "Returns a character which represents a given direction -- might
be useful for showing the movement along a path perhaps..."
  (cond ((= dir *n*) #\^)
	((= dir *s*) #\v)
	((= dir *e*) #\>)
	(t #\<)))

(defun maparray (function array &optional (same-type nil))
  "Maps function over array in its major order.  If SAME-TYPE,
then the new array will have the same element type as the old
array; but if a function returns an invalid element then an error
may occur.  If SAME-TYPE is NIL, then the array will accommodate
any type."
  (let ((new-array (apply #'make-array (array-dimensions array)
			  :element-type (if same-type
					    (array-element-type array)
					  t)
			  :adjustable (adjustable-array-p array)
			  (if (vectorp array)
			      `(:fill-pointer ,(fill-pointer array))
			    nil))))
    (dotimes (x (array-total-size array) new-array)
      (setf (row-major-aref new-array x)
	    (funcall function (row-major-aref array x))))))

(defun print-map (map)
  "Prints a map, which must be a 2D array.  If a value in the map
is T (indicating a space), then a '.' is printed.  If a value in the map
is NIL (indicating a food pellet), then a '#' is printed.  If a value in
the map is anything else, then it is simply PRINTed.  This allows you to
consider spaces to be all sorts of stuff in case you'd like to print a
trail of spaces on the map for example.  Returns NIL."
  (let ((dim (array-dimensions map)))
    (dotimes (y (second dim) nil)
      (format t "~%")
      (dotimes (x (first dim))
	(format t "~a"
		(let ((v (aref map x y)))
		  (cond ((equal v t) #\.)
			((null v) #\#)
			(t v))))))))


(defmacro absolute-direction (relative-dir ant-dir)
  "If the ant is facing ANT-DIR, then converts the perceived
RELATIVE-DIR direction into an absolute ('true') direction
and returns that."
  `(mod (+ ,relative-dir ,ant-dir) 4))

(defmacro x-pos-at (x-pos absolute-dir &optional (steps 1))
  "Returns the new x position if one moved STEPS steps the absolute-dir
direction from the given x position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *n*) ,x-pos)         ;; n or s
	      ((= ,absolute-dir *e*) (+ ,x-pos ,steps))     ;; e
	      (t (+ ,x-pos (- ,steps) *map-width*)))         ;; w
	*map-width*))

(defmacro y-pos-at (y-pos absolute-dir &optional (steps 1))
  "Returns the new y position if onee moved STEPS steps in the absolute-dir
direction from the given y position.  Toroidal."
  `(mod (cond ((= (mod ,absolute-dir 2) *e*) ,y-pos)        ;; e or w
	      ((= ,absolute-dir *s*) (+ ,y-pos ,steps))     ;; s
	      (t (+ ,y-pos (- ,steps) *map-height*)))       ;; n
	*map-height*))


(defparameter *current-move* 0 "The move # that the ant is at right now")
(defparameter *num-moves* 600 "How many moves the ant may make")
(defparameter *current-x-pos* 0 "The current X position of the ant")
(defparameter *current-y-pos* 0 "The current Y position of the ant")
(defparameter *current-ant-dir* *e* "The current direction the ant is facing")
(defparameter *eaten-pellets* 0 "How many pellets the ant has eaten so far")
(defparameter *map* (make-map *map-strs*) "The ant's map")
(defparameter *alpha-counter* 0)
(defparameter *alphabet* '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

(defun get-next-alphabet ()
	   (let (res)
	     (if (>= *alpha-counter* (length *alphabet*))
		 (setf *alpha-counter* 0))
	     (setf res (elt *alphabet* *alpha-counter*))
	     (incf *alpha-counter*)
	     res))

(defmacro if-food-ahead (then else)
	   "If there is food directly ahead of the ant, then THEN is evaluated, else ELSE is evaluated"
	   ;; because this is an if/then statement, it MUST be implemented as a macro.
	   `(if ,(null (aref *map* (y-pos-at *current-y-pos* *current-ant-dir* ) (x-pos-at *current-x-pos* *current-ant-dir* )))
		,then
		,else))

(defun progn2 (arg1 arg2)
	   "Evaluates arg1 and arg2 in succession, then returns the value of arg2"
	   (eval arg1)
	   (eval arg2)
	   arg2)

(defun progn3 (arg1 arg2 arg3)
	   "Evaluates arg1, arg2, and arg3 in succession, then returns the value of arg3"
	   (eval arg1)
	   (eval arg2)
	   (eval arg3)
	   arg3)

(defun move ()
	   "If the move count does not exceed *num-moves*, increments the move count
and moves the ant forward, consuming any pellet under the new square where the
ant is now.  Perhaps it might be nice to leave a little trail in the map showing
where the ant had gone."
	     (when (< *current-move* *num-moves*)
	       (setf (aref *map* *current-y-pos* *current-x-pos*) (get-next-alphabet)) ;;mark the map where ant had gone
	       (setf *current-move* (1+ *current-move*))
	       (setf *current-x-pos* (x-pos-at *current-x-pos* *current-ant-dir*)) 
	       (setf *current-y-pos* (y-pos-at *current-y-pos* *current-ant-dir*))
	       (when (null (aref *map* *current-y-pos* *current-x-pos*)) ;;find a pellet
		 (setf *eaten-pellets* (1+ *eaten-pellets*)))))


(defun left ()
	   "Increments the move count, and turns the ant left"
	   (when (< *current-move* *num-moves*)
	     (setf *current-move* (1+ *current-move*))
	     (setf *current-ant-dir* (absolute-direction -1 *current-ant-dir*))))

(defun right ()
	   "Increments the move count, and turns the ant right"
	   (when (< *current-move* *num-moves*)
	     (setf *current-move* (1+ *current-move*))
	     (setf *current-ant-dir* (absolute-direction 1 *current-ant-dir*))))

(defparameter *nonterminal-set* nil)
(defparameter *terminal-set* nil)

(defun gp-artificial-ant-setup ()
  "Sets up vals"
  (setq *nonterminal-set* '((if-food-ahead 2) (progn2 2) (progn3 3)))
  (setq *terminal-set* '(left right move))
  (setq *map* (make-map *map-strs*))
  (setq *current-move* 0)
  (setq *eaten-pellets* 0))

(defun gp-artificial-ant-evaluator (ind)
	   "Evaluates an individual by putting it in a fresh map and letting it run
for *num-moves* moves.  The fitness is the number of pellets eaten -- thus
more pellets, higher (better) fitness."
	   (setf *current-move* 0)
	   (setf *num-moves* 600)
	   (setf *current-x-pos* 0)
	   (setf *current-y-pos* 0)
	   (setf *current-ant-dir* *e*)
	   (setf *eaten-pellets* 0)
	   (setf *alpha-counter* 0)
	   (setf *map* (make-map *map-strs*))
	   (while (< *current-move* *num-moves*)
		  (eval ind))
	   *eaten-pellets*)

#|
(evolve 50 500
 		 :setup #'gp-artificial-ant-setup
		 :creator #'gp-creator
		 :selector #'tournament-selector
		 :modifier #'gp-modifier
		 :evaluator #'gp-artificial-ant-evaluator
		 :printer #'simple-printer)

Best Fitness: 89
Best Individual:
'(progn3
(if-food-ahead (move)
 (if-food-ahead (if-food-ahead (progn2 (left) (left)) (move))
  (progn3 (left) (left)
   (if-food-ahead (move)
    (if-food-ahead (left) (left))))))
(move) (left))

|#