
;;; Some utility Functions and Macros that you might find to be useful (hint)

(defmacro while (test &rest body)     
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

(defun random? (&optional (prob 0.5))     
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))


;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 
(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
  (let* ((index (random (length population))) (best (elt population index)) (best-value (elt fitnesses index))) ;; index : random picked number. Then, use index to access best and its value.
    (loop for i from 2 to *tournament-size*
	  do (setf index (random (length population))) ;; Pick a random number for Next
	  when (> (elt fitnesses index) best-value ) ;;Condition : Fitness(Next) > Fitness(Best)
	    do (setf best (elt population index))
	    and do (setf best-value (elt fitnesses index)))
    best)
)


(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"
  (let ((selections '())) ;; a list contains selected individuals.
    (loop repeat num
	  do (push (tournament-select-one population fitnesses) selections))
   selections)
)


;; I'm nice and am providing this for you.  :-)
(defun simple-printer (pop fitnesses)
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


(defun evolve (generations pop-size
	       &key setup creator selector modifier evaluator printer)
  (funcall setup) ;;setup the global variables ;;function call (setup)
  (let ((p '()) (best '()) (fitnesses '()) (q '()))   ;; variables
    (loop for i from 1 to pop-size  ;; create individuals
	  do (push (funcall creator) p))  ;;function call (creator)
    
    (setf fitnesses (mapcar #'(lambda (x) (funcall evaluator x)) p)) ;;Accessfitness ;;function call (evaluator)
    (let ((i 0))
      (while (< i generations) ;;repeat generations times
	     (setf best (car p)) ;;use first individual as temporary best.
	     (dotimes (x (length p)) ;;find the best
	       (when (> (elt fitnesses x) (funcall evaluator best) ) ;;function call (evaluator)
		 (setf best (elt p x))))
	     (setf q '()) ;;reset Q
	     (let ((pab '()) (cab '()) ) ;;pab : selected parents. cab : new children
	       (loop repeat (/ pop-size 2)  ;;run pop-size/2 times
		     do (setf pab (funcall selector 2 p fitnesses)) ;; select Pa and Pb  ;;function call (selector)
		     do (setf cab  (funcall modifier (car pab) (car (cdr pab)))) ;;modify Pa&Pb to get Ca&Cb ;;function call (modifier)
		     do (push (car cab) q)  ;;put Ca in Q
		     do (push (car (cdr cab)) q)) ;;put Cb in Q
	       )
	     (setf p q) ;; P <- Q
	     (setf fitnesses (mapcar #'(lambda (x) (funcall evaluator x)) p)) ;;Compute the new fitnesses of new p 
	     (funcall printer p fitnesses) ;;function call (printer)
	     (incf i)))
    NIL
    )
)


(defparameter *float-vector-length* 20 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
UNIFORM random numbers in the range appropriate to the given problem"
  (let ((generated-list (generate-list *float-vector-length* #'random?))) ;;create a list filled with T and NIL (use random?)
    (mapcar #'(lambda (x) (if (eq x t)(random *float-max*)(- (random *float-max*)))) generated-list)) ;;If T, positive random number. otherwise, negative random number
)

;; I just made up these numbers, you'll probably need to tweak them
(defparameter *crossover-probability* 0.1
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.1
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.02
  "Per-gene mutation variance in gaussian convolution")

;; to impement FLOAT-VECTOR-MODIFIER, the following two functions are
;; strongly reccommended.

(defun uniform-crossover (ind1 ind2)

  (dotimes (i (length ind1))
    (when (>= *crossover-probability* (random 1.0))
      (rotatef (elt ind1 i) (elt ind2 i) ))
    )
)

(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."
  (dotimes (i (length ind)) ;;from 1 to len
    (when (>= *mutation-probability* (random 1.0))
      (let ((n (gaussian-random 0 *mutation-variance*))) ;; n is random number
	(while (or (< (+ (elt ind i) n) *float-min*) (> (+ (elt ind i) n) *float-max*)) ;;condition : if not between  max and min
	       (setf n (gaussian-random 0 *mutation-variance*))
	       )
	(setf (nth i ind) (+ (elt ind i) n) ))))
)

(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

  (let ((c  (list (copy-list ind1) (copy-list ind2)))) ;;copy Pa Pb
    (uniform-crossover (car c) (car (cdr c)))  ;;crossover
    (gaussian-convolution (car c))  ;;mutate Pa
    (gaussian-convolution (car (cdr c)))  ;;mutate Pb
    c) ;;return (Ca, Cb)
)

;; set new value to global variables
(defun float-vector-sum-setup ()
  (setf *tournament-size* 7)
  (setf *crossover-probability* 0.1)
  (setf *mutation-probability* 0.1)
  (setf *mutation-variance* 0.02)
  )


;;; FITNESS EVALUATION FUNCTIONS

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



;;; an example way to fire up the GA.  If you've got it tuned right, it should quickly
;;; find individuals which are all very close to +5.12
#|
(evolve 50 1000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'sum-f
	:printer #'simple-printer)
|#

;;;;;Report
#|
0. If you want to change the global variables like *tournament-size*, you have to change them in function "float-vector-sum-setup".
   I put the global variables that would be change in that function, and function "evolve" will setup them at the beginning of execution.
   

1. Use rosenbrock-f. 
(evolve 50 1000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'rosenbrock-f
	:printer #'simple-printer)
a. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) : 
     I tried generations=50, 100, 200. Not much different between their best fitnesses.
     The fitness is around two values, -80.0 and -17.0.
--- test b ~   are with fixed generations=50 and pop-size=1000 ---
b. With (tournament-size=100, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) : 
     The result is the same as a.
c. With (tournament-size=7, crossover-probability=0.5, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around -70.0 and -15.0. -15.0 is more often than -70.0.
d. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.5, mutation-variance=0.02) : 
     The fitness is around -110.0 and -34.0. -34.0 is more often than -108.0.
e. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.1) : 
     The fitness is around -78.0 and -20.0. -20.0 is more often than -78.0.

2. Use rastrigin-f
(evolve 50 1000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'rastrigin-f
	:printer #'simple-printer)
a. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) :
     I tried generations=50, 100, 200. Each fitness is around -6.0.
b. With (tournament-size=100, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around -30.0.
c. With (tournament-size=7, crossover-probability=0.5, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around -1.5.
d. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.5, mutation-variance=0.02) : 
     The fitness is around -60.0.
e. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.1) : 
     The fitness is around -7.5.

3. Use sphere-f
(evolve 50 1000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'sphere-f
	:printer #'simple-printer)
a. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) :
     generations=50. Each fitness is around -0.005.
     generations=100. Each fitness is around -0.0012.
     generations=200. Each fitness is around -7.5e-4.
b. With (tournament-size=100, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around -2.0e-4.
c. With (tournament-size=7, crossover-probability=0.5, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around -0.0022.
d. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.5, mutation-variance=0.02) : 
     The fitness is around -0.16.
e. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.1) : 
     The fitness is around -0.018.

4. Use schwefel-f
(evolve 50 1000
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'schwefel-f
	:printer #'simple-printer)
a. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) :
     I tried generations=50, 100, 200. Each fitness is around 8340.
b. With (tournament-size=100, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around 7300.
c. With (tournament-size=7, crossover-probability=0.5, mutation-probability=0.1, mutation-variance=0.02) : 
     The fitness is around 8370.
d. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.5, mutation-variance=0.02) : 
     The fitness is around 8180.
e. With (tournament-size=7, crossover-probability=0.1, mutation-probability=0.1, mutation-variance=0.1) : 
     The fitness is around 8365.

|#


#|
I spent more time comprehending all the algorithms than implementing the code. 
All the functions are really straightforward after I understood how these functions work. 
While coding, I struggled with loop functions such as ¡§loop for¡¨, ¡§loop repeat¡¨, ¡§dotimes¡¨. 
I wanted to put condition ¡¨when¡¨ within a loop function, but they have different structures. 
For example, I have to add ¡§do¡¨ in front of an instruction in ¡§loop for¡¨ and ¡§loop repeat¡¨, but not for ¡§dotimes¡¨.
Therefore, I tried each one to figure out which one is better. 
Also, this is my first time to use ¡§nth¡¨ and ¡§elt¡¨ in assignments because ¡§car¡¨, ¡§cdr¡¨, ¡§nth¡¨ are enough to handle previous homework. 
They're pretty straightforward to access a element in a list or a sequence.
|#
