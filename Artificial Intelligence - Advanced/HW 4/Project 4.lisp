(defun negate (predicate)
	   "Negates a predicate.  Pretty simple!"
	   (cons (not (first predicate)) (rest predicate)))

;;;;;; STRIPS OPERATORS
;;;;;; A strips operator has is described below.  Note that
;;;;;; UNIQ allows two operators with the same name, preconds
;;;;;; and effects to be in the plan at once.

(defstruct operator
	   "Defines a strips operator consisting of
a NAME (a symbol or string),
a UNIQ gensym symbol assigned when the operator is added to the plan,
a list of PRECONDITIONS (predicates)
a list of EFFECTS ( predicates),

The resultant function MAKE-OPERATOR creates a *template*,
which is different from an instantiated operator actually in a plan.
Use instantiate-operator to create an operator from a template."
	   name uniq preconditions effects)


(defun copy-operator (operator)
	   "Copies the operator and assigns it a new unique gensym symbol to make
it unique as far as EQUALP is concerned.  Returns the copy."
	   (let ((op (copy-structure operator)))
	     (setf (operator-uniq op) (gensym))
	     op))



;;;;;;; LINKS
;;;;;;; A link is a structure that specifies a causal link with a from-operator,
;;;;;;; a to-operator, and precondition of the to-operator involved
;;;;;;; (which is the SAME as the effect of the from-operator!)

(defstruct (link
		     (:print-function print-link))
	   "FROM and TO are operators in the plan.
  PRECOND is the predicate that FROM's effect makes true in TO's precondition."
	   from precond to)

(defun print-link (p stream depth)
	   "Helper function to print link in a pretty way"
	   (declare (ignorable depth))
	   (format stream "#< (~a)~a -> (~a)~a : ~a >"
		   (when (link-from p) (operator-uniq (link-from p)))
		   (when (link-from p) (operator-name (link-from p)))
		   (when (link-to p) (operator-uniq (link-to p)))
		   (when (link-to p) (operator-name (link-to p)))
		   (link-precond p)))


;;;;;;; ORDERINGS
;;;;;;; An ordering is just a dotted pair of the form (before-op . after-op)
;;;;;;; where before-op and after-op are strips operators (instances of
;;;;;;; the OPERATOR structure).  The ordering specifies
;;;;;;; that before-op must come before after-op.


(defun print-ordering (p stream depth)
	   "Helper function to print link in a pretty way"
	   (declare (ignorable depth))
	   (format stream "#[ (~a)~a -> (~a)~a ]"
		   (operator-uniq (first p))
		   (operator-name (first p))
		   (operator-uniq (rest p))
		   (operator-name (rest p))))


;;;;;;; PLANS
;;;;;;; A plan is a list of operators, a list of orderings, and a list of
;;;;;;; links, plus the goal and start operator (which are also in the operator
;;;;;;; list).

(defstruct (plan (:print-function print-plan))
	   "A collection of lists of operators, orderings, and links,
plus a pointer to the start operator and to the goal operator."
	   operators orderings links start goal)

(defun print-plan (p stream depth)
	   "Helper function print plan in a pretty way"
	   (declare (ignorable depth))
	   (format stream "#< PLAN operators: ~{~%~a~} ~%links: ~{~%~a~} ~%orderings: ~{~%~a~}~%>"
		   (plan-operators p) (plan-links p)
		   (mapcar #'(lambda (ordering)
			       (print-ordering ordering nil 0))
			   (plan-orderings p))))

(defun copy-plan (plan)
	   "Deep-copies the plan, and copies the operators, orderings, and links."
	   (let ((p (copy-structure plan)))
	     (setf (plan-operators p) (copy-tree (plan-operators p)))
	     (setf (plan-orderings p) (copy-tree (plan-orderings p)))
	     (setf (plan-links p) (copy-tree (plan-links p)))
	     p))




;;;;;;;;; UTILITY FUNCTIONS
;;;;;;;;; I predict you will find these functions useful.

;;;; Reachable takes an association list and determines if you can reach
;;;; an item in the list from another item.  For example:
;;;;
;;;; (reachable '((a . b) (c . d) (b . c) (b . e) (e . a)) 'e 'd)
;;;; --> T   ;; e->a, a->b, b->c, c->d

;; optimized
(defun reachable (assoc-list from to)
	   "Returns t if to is reachable from from in the association list."
	   (if (or (not (find from (mapcar #'first assoc-list)))
		   (not (find to (mapcar #'rest assoc-list))))
	       (return-from reachable nil))
	   (let ((items-reachable (remove from assoc-list :key #'first :test-not #'equalp))
		 (items-not-reachable (remove from assoc-list :key #'first :test #'equalp))
		 temp)
	     (loop while (and items-reachable items-not-reachable)
		   do (progn
			(if (find to items-reachable :key #'rest)
			    (return-from reachable t))
			(dolist (item (copy-seq items-reachable))
			  (setf temp (length items-reachable))
			  (setf items-reachable (append (remove (rest item) items-not-reachable :key #'first :test-not #'equalp) items-reachable))
			  (setf items-reachable (remove item items-reachable))
			  (setf items-not-reachable (remove (rest item) items-not-reachable :key #'first)))))
	     (if (find to items-reachable :key #'rest)
		 (return-from reachable t)
		 (return-from reachable nil))))

;;;; Cyclic-assoc-list takes an association list and determines if it
;;;; contains a cycle (two objects can reach each other)
;;;;
;;;; (cyclic-assoc-list '((a . b) (c . d) (b . c) (b . e) (e . a)))
;;;; --> T   ;; a->b, b->e, e->a
;;;;
;;;; (cyclic-assoc-list '((a . a)))
;;;; --> T   ;; a->a

(defun cyclic-assoc-list (assoc-list)
	   (dolist (x assoc-list nil)
	     (when (reachable assoc-list (cdr x) (car x))
	       (return t))))

;;;; Binary-combinations returns all N^2 combinations of T and NIL.
;;;;
;;;; (binary-combinations 4)
;;;; -->
;;;; ((NIL T NIL T) (T T NIL T)
;;;;  (NIL NIL NIL T) (T NIL NIL T)
;;;;  (NIL T T T) (T T T T)
;;;;  (NIL NIL T T) (T NIL T T)
;;;;  (NIL T NIL NIL) (T T NIL NIL)
;;;;  (NIL NIL NIL NIL) (T NIL NIL NIL)
;;;;  (NIL T T NIL) (T T T NIL)
;;;;  (NIL NIL T NIL) (T NIL T NIL))

(defun binary-combinations (n)
	   "Gives all combinations of n t's and nils"
	   (let ((bag '(())))
	     (dotimes (x n bag)
	       (let (bag2)
		 (dolist (b bag)
		   (push (cons t b) bag2)
		   (push (cons nil b) bag2))
		 (setf bag bag2)))))



;;;;;; PLANNING CODE TEMPLATES

(defun before-p (operator1 operator2 plan)
	   "Operator1 is ordered before operator2 in plan?"
	   ;;if op1 -> op2 is reachabe, op1 is before op2
	   (reachable (plan-orderings plan) operator1 operator2))


(defun link-exists-for-precondition-p (precond operator plan)
	   "T if there's a link for the precond for a given operator, else nil.
precond is a predicate."
	   ;;check each link in plan
	   (dolist (link (plan-links plan))
	     ;;compare precond and to in link to the ones passed in.
	     (if (and (equalp (link-precond link) precond)  (equalp (link-to link) operator))
		 ;;if there is a link, return 1 as ture
		 (return-from link-exists-for-precondition-p t)))
	   nil)

(defun operator-threatens-link-p (operator link plan)
	   "T if operator threatens link in plan, because it's not ordered after
or before the link, and it's got an effect which counters the link's effect."
	   ;;if operator is the same as "from" operator or "to" operator in link, return nil because no threat
	   (if (or (equalp operator (link-from link)) (equalp operator (link-to link)))
	       (return-from operator-threatens-link-p nil))
	   ;; if operator is before link  or after link, return nil because no threat
	   (if (or (before-p operator (link-from link) plan) (before-p (link-to link) operator plan))
	       (return-from operator-threatens-link-p nil))
	   ;;else :  check countered effect
	   ;;get the negated operator's effects first.
	   ;;chech each operator's effect
	   (dolist (predicate (mapcar #'negate (operator-effects operator)))
	     ;;if an negated effect of operator is in link's effects, return true because there is a threat
	     (if (or (find predicate (operator-preconditions (link-to link)) :test #'equalp)
		     (find predicate (operator-effects (link-to link)) :test #'equalp))
		 (return-from operator-threatens-link-p t)))
	   ;;return nil because no threat
	   nil)

(defun inconsistent-p (plan)
	   "Plan orderings are inconsistent"
	   (cyclic-assoc-list (plan-orderings plan)))

(defun pick-precond (plan)
	   "Return ONE (operator . precondition) pair in the plan that has not been met yet.
If there is no such pair, return nil"
	   ;;put all pair that has not been met yet into a list, then random pick one from the list
	   (let ((pair-list '()))
	     ;;each operator in plan
	     (dolist (operator (plan-operators plan))
	       ;;each precondition of the operator
	       (dolist (precond (operator-preconditions operator))
		 ;;if there's no link that links to the precond and operator, this pair has not been met yet. Push pair into list
		 (if (null (link-exists-for-precondition-p precond operator plan))
		     (push (cons operator precond) pair-list))))
	     ;;random pick one or nil
	     (if pair-list
		 (elt (sort pair-list '< :key (lambda (x) (+ (length (gethash (rest x) *operators-for-precond*)) (length (operator-preconditions (first x)))))) 0)
		 nil)))

(defun all-effects (precondition plan)
	   "Given a precondition, returns a list of ALL operators presently IN THE PLAN which have
effects which can achieve this precondition."
	   (let ((operator-list '()))
	     ;;chech each plan's operators
	     (dolist (x (plan-operators plan) operator-list)
	       ;;if there is a operator's effect equal to precondition, push operator into list
	       (if (find precondition (operator-effects x) :test #'equalp)
		   (push x operator-list)))))

(defun all-operators (precondition)
	   "Given a precondition, returns all list of ALL operator templates which have
an effect that can achieve this precondition."
	   (gethash precondition *operators-for-precond*))

(defun select-subgoal (plan current-depth max-depth)
	   "For all possible subgoals, recursively calls choose-operator
on those subgoals.  Returns a solved plan, else nil if not solved."
	   ;;check current-depth
	   (if (>= current-depth max-depth)
	       (return-from select-subgoal nil) ;;exceed -> return nil
	       (incf current-depth)) ;;else increment current-depth before call choose-operator
	   (let ((subgoal (pick-precond plan)))
	     ;;if no subgoal ( function "pick-precond" returns nil ), plan is complete
	     (if (null subgoal)
		 (return-from select-subgoal plan))
	     ;;call choose-operator
	     (choose-operator subgoal plan current-depth max-depth)))


(defun choose-operator (op-precond-pair plan current-depth max-depth)
	   "For a given (operator . precondition) pair, recursively call
hook-up-operator for all possible operators in the plan.  If that
doesn't work, recursively call add operators and call hook-up-operators
on them. Returns a solved plan, else nil if not solved."
	   (let ((all-possible-operators (all-operators (cdr op-precond-pair))) ;;all possible operators (template)
		 (possible-operators-in-plan (all-effects (cdr op-precond-pair) plan)) ;;possible operators in plan (instance)
		 (possible-operators-out-plan '())  ;;possible operators not in plan (template)
		 copied-op final-plan)

	     ;;find the possible operators not in the plan (template)
	     (dolist (x all-possible-operators)
	       (if (not (find (operator-name x) possible-operators-in-plan :test #'equalp :key #'operator-name))
		   (push x possible-operators-out-plan)))

	     ;;try possible operators in plan first (instance)
	     (dolist (operator possible-operators-in-plan)
               ;;hook-up-operator    from             to               precondition                                 new-operator-was-added
	       (setf final-plan (hook-up-operator operator (car op-precond-pair) (cdr op-precond-pair) (copy-plan plan) current-depth max-depth operator))
	       ;;if a solved plan is found, return it.
	       (if final-plan
		   (return-from choose-operator final-plan)))

	     ;;try possible operators not in plan (template)
	     (dolist (operator possible-operators-out-plan)
	       ;;due to just template, copy the operator
	       (setf copied-op (copy-operator operator))
               ;;hook-up-operator      from          to           precondition                                 new-operator-was-added
	       (setf final-plan (hook-up-operator copied-op (car op-precond-pair) (cdr op-precond-pair) (copy-plan plan) current-depth max-depth copied-op))
	       (if final-plan
		   (return-from choose-operator final-plan)))
	     ;;return final plan : a solved plan or nil
	     nil))

(defun add-operator (operator plan)
	   "Given an OPERATOR and a PLAN makes a copy of the plan [the
operator should have already been copied out of its template at this point].
Then adds that copied operator
the copied plan, and hooks up the orderings so that the new operator is
after start and before goal.  Returns the modified copy of the plan."
	   ;;copy a new plan
	   (let ((new-plan (copy-plan plan)))
	     ;;add operator
	     (pushnew operator (plan-operators new-plan) :test #'equalp)
	     new-plan))

(defun hook-up-operator (from to precondition plan
				  current-depth max-depth
				  new-operator-was-added)
	   "Hooks up an operator called FROM, adding the links and orderings to the operator
TO for the given PRECONDITION that FROM achieves for TO.  Then
recursively  calls resolve-threats to fix any problems.  Presumes that
PLAN is a copy that can be modified at will by HOOK-UP-OPERATOR. Returns a solved
plan, else nil if not solved."
	   ;;create a new link
	   (let ((added-link (make-link :from from :precond precondition :to to)) threat-list)
	     ;;add link
	     (pushnew added-link (plan-links plan) :test #'equalp)
	     ;;add order
	     (pushnew (cons from to) (plan-orderings plan) :test #'equalp)
	     ;;if new-operator is not in plan's operators
	     (if (not (find (operator-name new-operator-was-added) (plan-operators plan) :test #'equalp :key #'operator-name))
		 (progn
		   ;;add operator (call "add-operator")
		   (setf plan (add-operator new-operator-was-added plan))
		   ;;add order start goal
		   (pushnew (cons (find 'start (plan-operators plan) :key #'operator-name ) new-operator-was-added) (plan-orderings plan) :test #'equalp)
		   (pushnew (cons new-operator-was-added (find 'goal (plan-operators plan) :key #'operator-name )) (plan-orderings plan) :test #'equalp)
		   ;;check threats (new operator) and (new link)
		   (setf threat-list (threats plan new-operator-was-added added-link)))
		 ;;else condition : new-operator is in plan's operators
		 ;;check threats : if operator is already in plan, pass in nil
		 (setf threat-list (threats plan nil added-link)))
	     ;;after getting the threat-list, call resolve-threats
	     (resolve-threats plan threat-list current-depth max-depth)))

(defun threats (plan maybe-threatening-operator maybe-threatened-link)
	   "After hooking up an operator, we have two places that we need to check for threats.
First, we need to see if the link we just created is threatened by some operator.
Second, IF we just added in an operator, then we need to check to see if it threatens
any links.

This function should return a list of (op . link) pairs (called ''threats'') which
indicate all the situations where some operator OP threatens a link LINK.  The only
situations you need to check are the ones described in the previous paragraph.

This function should assume that if MAYBE-THREATENING-OPERATOR is NIL, then no
operator was added and we don't have to check for its threats.  However, we must
always check for any operators which threaten MAYBE-THREATENED-LINK."


	   (let ((threat-list '()))
	     ;;Fist condition : added operator and plan's links
	     ;;operator is not nil
	     (if maybe-threatening-operator
		 (progn
		   ;;check maybe-threatening-operator to each link in plan
		   (dolist (link (plan-links plan))
		     ;;if funcall return T, push pair into list
		     (if (operator-threatens-link-p maybe-threatening-operator link plan)
			 (pushnew (cons maybe-threatening-operator link) threat-list :test #'equalp)))))

	     ;;Second condition : added link and plan's operators
	     ;;check maybe-threatened-link to each operator in plan
	     (dolist (operator (plan-operators plan))
	       ;;if funcall return T, push pair into list
	       (if (operator-threatens-link-p operator maybe-threatened-link plan)
		   (pushnew (cons operator maybe-threatened-link) threat-list :test #'equalp)))
	     ;;return threat list
	     threat-list))


(defun all-promotion-demotion-plans (plan threats)
"Returns plans for each combination of promotions and demotions
of the given threats, except  for the inconsistent plans.  These plans
are copies of the original plan."
	   (let ((plan-list '()))
	     ;;check the number of threats
	     (if (equalp (length threats) 1)
		 ;;only one threat
		 (progn
		   (let ((threat '()) (copied-plan '()))
		     ;;get the threat
		     (setf threat (car threats))
		     ;;copy a plan, then promote threat.
		     (setf copied-plan (copy-plan plan))
		     (promote (car threat) (cdr threat) copied-plan)
		     ;;no cycle, then push into plan-list
		     (if (not (inconsistent-p copied-plan))
			 (push copied-plan  plan-list))

		     ;;copy a plan, then demote threat.
		     (setf copied-plan (copy-plan plan))
		     (demote  (car threat) (cdr threat) copied-plan)
		     ;;no cycle, then push into plan-list
		     (if (not (inconsistent-p copied-plan))
			 (push copied-plan  plan-list))));progn
		 ;;more than one threat
		 (progn
		   ;;use function "binary-combinations" to get all possible combinations
		   (let ((combinations (binary-combinations (length threats))) (copied-plan '()))
		     (dolist (combination combinations)
		       ;;copy a plan
		       (setf copied-plan (copy-plan plan))
		       ;;use mapc, ex : (nil T T) (threat-1 threat-2 threat-3). nil=>use demote, T=>use promote
		       (mapcan #'(lambda (x y)  (if (null x) (demote (car y) (cdr y) copied-plan) (promote (car y) (cdr y) copied-plan) )) combination threats)
		       ;;no cycle, then push into plan-list
		       (if (not (inconsistent-p copied-plan))
			   (push copied-plan  plan-list))))))
	     plan-list))

(defun promote (operator link plan)
	   "Promotes an operator relative to a link.  Doesn't copy the plan."
	   (pushnew (cons operator (link-from link)) (plan-orderings plan) :test #'equalp)
	   plan)

(defun demote (operator link plan)
	   "Demotes an operator relative to a link.  Doesn't copy the plan."
	   (pushnew (cons (link-to link) operator) (plan-orderings plan) :test #'equalp)
	   plan)

(defun resolve-threats (plan threats current-depth max-depth)
	   "Tries all combinations of solutions to all the threats in the plan,
then recursively calls SELECT-SUBGOAL on them until one returns a
solved plan.  Returns the solved plan, else nil if no solved plan."
	   ;;check if threats exist
	   (if (null threats)
	       ;;no threat, then call SELECT-SUBGOAL directly
	       (select-subgoal plan current-depth max-depth)
	       ;;else, solve the threats ( call "all-promotion-demotion-plans" ), then call SELECT-SUBGOAL on a list of plans
	       (progn
		 (let (temp-plan)
		   ;;check each combination of solution plan
		   (dolist (new-plan (all-promotion-demotion-plans plan threats))
		     (setf temp-plan (select-subgoal new-plan current-depth max-depth))
		     ;;if temp-plan is a solved plan, return.
		     (if temp-plan
			 (return-from resolve-threats temp-plan)))))))




;;;;;;; DO-POP
;;;;;;; This is the high-level code.  Note it creates a goal and a start
;;;;;;; operator, then creates a plan with those operators and an ordering
;;;;;;; between them.  Then it does iterative-deepening, calling
;;;;;;; SELECT-SUBGOAL with ever-larger maximum depths.  If the solution is
;;;;;;; non-null, it breaks out of the loop and returns the solution.

(defparameter *depth-increment* 1
  "The depth to increment in iterative deepening search")

;;; This is used to cache the operators by precond.  You might find this
;;; useful to make your ALL-OPERATORS code much much much faster than the
;;; obvious dorky way to do it.
(defparameter *operators-for-precond* nil
  "Hash table.  Will yield a list of operators which can achieve a given precondition")

(defun build-operators-for-precond ()
  "Builds the hash table"
  (setf *operators-for-precond* (make-hash-table :test #'equalp))
  (dolist (operator *operators*)
    (dolist (effect (operator-effects operator))
      (push operator (gethash effect *operators-for-precond*)))))


(defun do-pop ()
  (let* ((start (make-operator
                 :name 'start
                 :uniq (gensym)
                 :preconditions nil
                 :effects *start-effects*))
         (goal (make-operator
                :name 'goal
                :uniq (gensym)
                :preconditions *goal-preconditions*
                :effects nil))
         (plan (make-plan
                :operators (list start goal)
                :orderings (list (cons start goal))
                :links nil
                :start start
                :goal goal))
         (depth *depth-increment*)
         solution)
    (build-operators-for-precond)
    ;; Do iterative deepening search on this sucker
    (loop
       (format t "~%Search Depth: ~d" depth)
       (setf solution (select-subgoal plan 0 depth))
       (when solution (return)) ;; break from loop, we're done!
       (incf depth *depth-increment*))
    ;; found the answer if we got here
    (format t "~%Solution Discovered:~%~%")
    solution))





;;;;; TWO-BLOCK-WORLD

(defparameter *operators*
  (list
   ;; move from table operators
   (make-operator :name 'a-table-to-b
                  :preconditions '((t a-on-table) (t b-clear) (t a-clear))
                  :effects '((nil a-on-table) (nil b-clear) (t a-on-b)))
   (make-operator :name 'b-table-to-a
                  :preconditions '((t b-on-table) (t a-clear) (t b-clear))
                  :effects '((nil b-on-table) (nil a-clear) (t b-on-a)))
   ;; move to table operators
   (make-operator :name 'a-b-to-table
                  :preconditions '((t a-on-b) (t a-clear))
                  :effects '((t a-on-table) (nil a-on-b) (t b-clear)))
   (make-operator :name 'b-a-to-table
                  :preconditions '((t b-on-a) (t b-clear))
                  :effects '((t b-on-table) (nil b-on-a) (t a-clear))))
  "A list of strips operators without their uniq gensyms set yet --
doesn't matter really -- but NOT including a goal or start operator")


;;; b is on top of a
(defparameter *start-effects*
  '((t a-on-table) (t b-on-a) (t b-clear)))

;;; a is on top of b
(defparameter *goal-preconditions*
  ;; somewhat redundant, is doable with just ((t a-on-b))
  '((t a-on-b) (t b-on-table) (t a-clear)))


;;;;; SOLUTION

;; * (time (do-pop))

;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Search Depth: 7
;; Search Depth: 8
;; Search Depth: 9
;; Solution Discovered:

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;   100.00% CPU
;;   3,126,524 processor cycles
;;   393,216 bytes consed

;; #< PLAN operators:
;; #S(OPERATOR
;;    :NAME A-TABLE-TO-B
;;    :UNIQ G28422
;;    :PRECONDITIONS ((T A-ON-TABLE) (T B-CLEAR) (T A-CLEAR))
;;    :EFFECTS ((NIL A-ON-TABLE) (NIL B-CLEAR) (T A-ON-B)))
;; #S(OPERATOR
;;    :NAME B-A-TO-TABLE
;;    :UNIQ G28421
;;    :PRECONDITIONS ((T B-ON-A) (T B-CLEAR))
;;    :EFFECTS ((T B-ON-TABLE) (NIL B-ON-A) (T A-CLEAR)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G28377
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T A-ON-TABLE) (T B-ON-A) (T B-CLEAR)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G28378
;;    :PRECONDITIONS ((T A-ON-B) (T B-ON-TABLE) (T A-CLEAR))
;;    :EFFECTS NIL)
;; links:
;; #< (G28377)START -> (G28422)A-TABLE-TO-B : (T A-ON-TABLE) >
;; #< (G28377)START -> (G28422)A-TABLE-TO-B : (T B-CLEAR) >
;; #< (G28421)B-A-TO-TABLE -> (G28422)A-TABLE-TO-B : (T A-CLEAR) >
;; #< (G28422)A-TABLE-TO-B -> (G28378)GOAL : (T A-ON-B) >
;; #< (G28421)B-A-TO-TABLE -> (G28378)GOAL : (T B-ON-TABLE) >
;; #< (G28377)START -> (G28421)B-A-TO-TABLE : (T B-ON-A) >
;; #< (G28377)START -> (G28421)B-A-TO-TABLE : (T B-CLEAR) >
;; #< (G28421)B-A-TO-TABLE -> (G28378)GOAL : (T A-CLEAR) >
;; orderings:
;; #[ (G28421)B-A-TO-TABLE -> (G28422)A-TABLE-TO-B ]
;; #[ (G28377)START -> (G28422)A-TABLE-TO-B ]
;; #[ (G28422)A-TABLE-TO-B -> (G28378)GOAL ]
;; #[ (G28377)START -> (G28421)B-A-TO-TABLE ]
;; #[ (G28421)B-A-TO-TABLE -> (G28378)GOAL ]
;; #[ (G28377)START -> (G28378)GOAL ]
;; >




;;;;; THREE-BLOCK-WORLD

;; (defparameter *operators*
;;   (list
;;    ;; move from table operators
;;    (make-operator :name 'a-table-to-b
;;                :preconditions '((t a-on-table) (t b-clear) (t a-clear))
;;                :effects '((nil a-on-table) (nil b-clear) (t a-on-b)))
;;    (make-operator :name 'a-table-to-c
;;                :preconditions '((t a-on-table) (t c-clear) (t a-clear))
;;                :effects '((nil a-on-table) (nil c-clear) (t a-on-c)))
;;    (make-operator :name 'b-table-to-a
;;                :preconditions '((t b-on-table) (t a-clear) (t b-clear))
;;                :effects '((nil b-on-table) (nil a-clear) (t b-on-a)))
;;    (make-operator :name 'b-table-to-c
;;                :preconditions '((t b-on-table) (t c-clear) (t b-clear))
;;                :effects '((nil b-on-table) (nil c-clear) (t b-on-c)))
;;    (make-operator :name 'c-table-to-a
;;                :preconditions '((t c-on-table) (t a-clear) (t c-clear))
;;                :effects '((nil c-on-table) (nil a-clear) (t c-on-a)))
;;    (make-operator :name 'c-table-to-b
;;                :preconditions '((t c-on-table) (t b-clear) (t c-clear))
;;                :effects '((nil c-on-table) (nil b-clear) (t c-on-b)))
;;    ;; move to table operators
;;    (make-operator :name 'a-b-to-table
;;                :preconditions '((t a-on-b) (t a-clear))
;;                :effects '((t a-on-table) (nil a-on-b) (t b-clear)))
;;    (make-operator :name 'a-c-to-table
;;                :preconditions '((t a-on-c) (t a-clear))
;;                :effects '((t a-on-table) (nil a-on-c) (t c-clear)))
;;    (make-operator :name 'b-a-to-table
;;                :preconditions '((t b-on-a) (t b-clear))
;;                :effects '((t b-on-table) (nil b-on-a) (t a-clear)))
;;    (make-operator :name 'b-c-to-table
;;                :preconditions '((t b-on-c) (t b-clear))
;;                :effects '((t b-on-table) (nil b-on-c) (t c-clear)))
;;    (make-operator :name 'c-a-to-table
;;                :preconditions '((t c-on-a) (t c-clear))
;;                :effects '((t c-on-table) (nil c-on-a) (t a-clear)))
;;    (make-operator :name 'c-b-to-table
;;                :preconditions '((t c-on-b) (t c-clear))
;;                :effects '((t c-on-table) (nil c-on-b) (t b-clear)))
;;    ;; block-to-block operators
;;    (make-operator :name 'a-b-to-c
;;                :preconditions '((t a-on-b) (t a-clear) (t c-clear))
;;                :effects '((nil a-on-b) (t a-on-c) (nil c-clear) (t b-clear)))
;;    (make-operator :name 'a-c-to-b
;;                :preconditions '((t a-on-c) (t a-clear) (t b-clear))
;;                :effects '((nil a-on-c) (t a-on-b) (nil b-clear) (t c-clear)))
;;    (make-operator :name 'b-a-to-c
;;                :preconditions '((t b-on-a) (t b-clear) (t c-clear))
;;                :effects '((nil b-on-a) (t b-on-c) (nil c-clear) (t a-clear)))
;;    (make-operator :name 'b-c-to-a
;;                :preconditions '((t b-on-c) (t b-clear) (t a-clear))
;;                :effects '((nil b-on-c) (t b-on-a) (nil a-clear) (t c-clear)))
;;    (make-operator :name 'c-a-to-b
;;                :preconditions '((t c-on-a) (t c-clear) (t b-clear))
;;                :effects '((nil c-on-a) (t c-on-b) (nil b-clear) (t a-clear)))
;;    (make-operator :name 'c-b-to-a
;;                :preconditions '((t c-on-b) (t c-clear) (t a-clear))
;;                :effects '((nil c-on-b) (t c-on-a) (nil a-clear) (t b-clear))))
;;   "A list of strips operators without their uniq gensyms set yet --
;; doesn't matter really -- but NOT including a goal or start operator")

;; (defparameter *start-effects*
;;   ;; Sussman Anomaly
;;   '((t a-on-table) (t b-on-table) (t c-on-a) (t b-clear) (t c-clear))
;;   "A list of predicates which specify the initial state")

;; ;; (defparameter *start-effects*
;; ;;   ;; another simple situation: all on table
;; ;;   '((t a-on-table) (t a-clear)
;; ;;     (t b-on-table) (t b-clear)
;; ;;     (t c-on-table) (t c-clear)))

;; (defparameter *goal-preconditions*
;;   '((t a-on-b) (t b-on-c) (t c-on-table) (t a-clear)))


;;;;; SOLUTION

;; * (time (do-pop))

;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME A-TABLE-TO-B
;;    :UNIQ G7165
;;    :PRECONDITIONS ((T A-ON-TABLE) (T B-CLEAR) (T A-CLEAR))
;;    :EFFECTS ((NIL A-ON-TABLE) (NIL B-CLEAR) (T A-ON-B)))
;; #S(OPERATOR
;;    :NAME B-TABLE-TO-C
;;    :UNIQ G7164
;;    :PRECONDITIONS ((T B-ON-TABLE) (T C-CLEAR) (T B-CLEAR))
;;    :EFFECTS ((NIL B-ON-TABLE) (NIL C-CLEAR) (T B-ON-C)))
;; #S(OPERATOR
;;    :NAME C-A-TO-TABLE
;;    :UNIQ G7163
;;    :PRECONDITIONS ((T C-ON-A) (T C-CLEAR))
;;    :EFFECTS ((T C-ON-TABLE) (NIL C-ON-A) (T A-CLEAR)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G3875
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T A-ON-TABLE) (T B-ON-TABLE) (T C-ON-A) (T B-CLEAR) (T C-CLEAR)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G3876
;;    :PRECONDITIONS ((T A-ON-B) (T B-ON-C) (T C-ON-TABLE) (T A-CLEAR))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G7163)C-A-TO-TABLE -> (G3876)GOAL : (T A-CLEAR) >
;; #< (G3875)START -> (G7165)A-TABLE-TO-B : (T B-CLEAR) >
;; #< (G7163)C-A-TO-TABLE -> (G7165)A-TABLE-TO-B : (T A-CLEAR) >
;; #< (G3875)START -> (G7164)B-TABLE-TO-C : (T C-CLEAR) >
;; #< (G3875)START -> (G7164)B-TABLE-TO-C : (T B-CLEAR) >
;; #< (G3875)START -> (G7163)C-A-TO-TABLE : (T C-CLEAR) >
;; #< (G3875)START -> (G7165)A-TABLE-TO-B : (T A-ON-TABLE) >
;; #< (G7165)A-TABLE-TO-B -> (G3876)GOAL : (T A-ON-B) >
;; #< (G3875)START -> (G7164)B-TABLE-TO-C : (T B-ON-TABLE) >
;; #< (G7164)B-TABLE-TO-C -> (G3876)GOAL : (T B-ON-C) >
;; #< (G3875)START -> (G7163)C-A-TO-TABLE : (T C-ON-A) >
;; #< (G7163)C-A-TO-TABLE -> (G3876)GOAL : (T C-ON-TABLE) > 
;; orderings: 
;; #[ (G7163)C-A-TO-TABLE -> (G7165)A-TABLE-TO-B ]
;; #[ (G7164)B-TABLE-TO-C -> (G7165)A-TABLE-TO-B ]
;; #[ (G3875)START -> (G7165)A-TABLE-TO-B ]
;; #[ (G7165)A-TABLE-TO-B -> (G3876)GOAL ]
;; #[ (G7163)C-A-TO-TABLE -> (G7164)B-TABLE-TO-C ]
;; #[ (G3875)START -> (G7164)B-TABLE-TO-C ]
;; #[ (G7164)B-TABLE-TO-C -> (G3876)GOAL ]
;; #[ (G3875)START -> (G7163)C-A-TO-TABLE ]
;; #[ (G7163)C-A-TO-TABLE -> (G3876)GOAL ]
;; #[ (G3875)START -> (G3876)GOAL ]
;; >




;;;;; SPARE-TIRE-PROBLEM
;; (defparameter *operators*
;;   (list
;;    (make-operator :name 'spare-remove-from-trunk
;;                   :preconditions '((t spare-at-trunk))
;;                   :effects '((nil spare-at-trunk) (t spare-at-ground)))
;;    (make-operator :name 'flat-remove-from-axle
;;                   :preconditions '((t flat-at-axle))
;;                   :effects '((nil flat-at-axle) (t flat-at-ground)))
;;    (make-operator :name 'spare-put-on-axle
;;                   :preconditions '((t spare-at-ground) (nil flat-at-axle))
;;                   :effects '((nil spare-at-ground) (t spare-at-axle)))
;;    (make-operator :name 'leave-overnight
;;                   :preconditions '()
;;                   :effects '((nil spare-at-ground) (nil spare-at-axle) (nil spare-at-trunk) (nil flat-at-ground) (nil flat-at-axle))))
;;   "A list of strips operators without their uniq gensyms set yet --
;; doesn't matter really -- but NOT including a goal or start operator")


;; ;;; flat at axle
;; (defparameter *start-effects*
;;   '((t flat-at-axle) (t spare-at-trunk)))

;; ;;; spare at axle
;; (defparameter *goal-preconditions*
;;   '((t spare-at-axle)))


;;;;; SOLUTION

;; * (time (do-pop))

;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Solution Discovered:

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;   100.00% CPU
;;   735,804 processor cycles
;;   131,072 bytes consed
  
;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME FLAT-REMOVE-FROM-AXLE
;;    :UNIQ G14612
;;    :PRECONDITIONS ((T FLAT-AT-AXLE))
;;    :EFFECTS ((NIL FLAT-AT-AXLE) (T FLAT-AT-GROUND)))
;; #S(OPERATOR
;;    :NAME SPARE-REMOVE-FROM-TRUNK
;;    :UNIQ G14611
;;    :PRECONDITIONS ((T SPARE-AT-TRUNK))
;;    :EFFECTS ((NIL SPARE-AT-TRUNK) (T SPARE-AT-GROUND)))
;; #S(OPERATOR
;;    :NAME SPARE-PUT-ON-AXLE
;;    :UNIQ G14610
;;    :PRECONDITIONS ((T SPARE-AT-GROUND) (NIL FLAT-AT-AXLE))
;;    :EFFECTS ((NIL SPARE-AT-GROUND) (T SPARE-AT-AXLE)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G14595
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T FLAT-AT-AXLE) (T SPARE-AT-TRUNK)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G14596
;;    :PRECONDITIONS ((T SPARE-AT-AXLE))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G14595)START -> (G14612)FLAT-REMOVE-FROM-AXLE : (T FLAT-AT-AXLE) >
;; #< (G14612)FLAT-REMOVE-FROM-AXLE -> (G14610)SPARE-PUT-ON-AXLE : (NIL
;;                                                                  FLAT-AT-AXLE) >
;; #< (G14595)START -> (G14611)SPARE-REMOVE-FROM-TRUNK : (T SPARE-AT-TRUNK) >
;; #< (G14611)SPARE-REMOVE-FROM-TRUNK -> (G14610)SPARE-PUT-ON-AXLE : (T
;;                                                                    SPARE-AT-GROUND) >
;; #< (G14610)SPARE-PUT-ON-AXLE -> (G14596)GOAL : (T SPARE-AT-AXLE) > 
;; orderings: 
;; #[ (G14611)SPARE-REMOVE-FROM-TRUNK -> (G14612)FLAT-REMOVE-FROM-AXLE ]
;; #[ (G14612)FLAT-REMOVE-FROM-AXLE -> (G14596)GOAL ]
;; #[ (G14595)START -> (G14612)FLAT-REMOVE-FROM-AXLE ]
;; #[ (G14612)FLAT-REMOVE-FROM-AXLE -> (G14610)SPARE-PUT-ON-AXLE ]
;; #[ (G14611)SPARE-REMOVE-FROM-TRUNK -> (G14596)GOAL ]
;; #[ (G14595)START -> (G14611)SPARE-REMOVE-FROM-TRUNK ]
;; #[ (G14611)SPARE-REMOVE-FROM-TRUNK -> (G14610)SPARE-PUT-ON-AXLE ]
;; #[ (G14595)START -> (G14610)SPARE-PUT-ON-AXLE ]
;; #[ (G14610)SPARE-PUT-ON-AXLE -> (G14596)GOAL ]
;; #[ (G14595)START -> (G14596)GOAL ]
;; >




;;;;; AIR-CARGO-TRANSPORT

;; (defparameter *operators*
;;   (list
;;     ;; load cargo
;;    (make-operator :name 'load-c1-to-p1-at-sfo
;;                   :preconditions '((t c1-at-sfo) (t p1-at-sfo))
;;                   :effects '((nil c1-at-sfo) (t c1-in-p1)))
;;    (make-operator :name 'load-c1-to-p1-at-jfk
;;                   :preconditions '((t c1-at-jfk) (t p1-at-jfk))
;;                   :effects '((nil c1-at-jfk) (t c1-in-p1)))
;;    (make-operator :name 'load-c1-to-p2-at-sfo
;;                   :preconditions '((t c1-at-sfo) (t p2-at-sfo))
;;                   :effects '((nil c1-at-sfo) (t c1-in-p2)))
;;    (make-operator :name 'load-c1-to-p2-at-jfk
;;                   :preconditions '((t c1-at-jfk) (t p2-at-jfk))
;;                   :effects '((nil c1-at-jfk) (t c1-in-p2)))
;;    (make-operator :name 'load-c2-to-p1-at-sfo
;;                   :preconditions '((t c2-at-sfo) (t p1-at-sfo))
;;                   :effects '((nil c2-at-sfo) (t c2-in-p1)))
;;    (make-operator :name 'load-c2-to-p1-at-jfk
;;                   :preconditions '((t c2-at-jfk) (t p1-at-jfk))
;;                   :effects '((nil c2-at-jfk) (t c2-in-p1)))
;;    (make-operator :name 'load-c2-to-p2-at-sfo
;;                   :preconditions '((t c2-at-sfo) (t p2-at-sfo))
;;                   :effects '((nil c2-at-sfo) (t c2-in-p2)))
;;    (make-operator :name 'load-c2-to-p2-at-jfk
;;                   :preconditions '((t c2-at-jfk) (t p2-at-jfk))
;;                   :effects '((nil c2-at-jfk) (t c2-in-p2)))

;;     ;; ul cargo
;;    (make-operator :name 'unload-c1-from-p1-at-sfo
;;                   :preconditions '((t c1-in-p1) (t p1-at-sfo))
;;                   :effects '((t c1-at-sfo) (nil c1-in-p1)))
;;    (make-operator :name 'unload-c1-from-p1-at-jfk
;;                   :preconditions '((t c1-in-p1) (t p1-at-jfk))
;;                   :effects '((t c1-at-jfk) (nil c1-in-p1)))
;;    (make-operator :name 'unload-c1-from-p2-at-sfo
;;                   :preconditions '((t c1-in-p2) (t p2-at-sfo))
;;                   :effects '((t c1-at-sfo) (nil c1-in-p2)))
;;    (make-operator :name 'unload-c1-from-p2-at-jfk
;;                   :preconditions '((t c1-in-p2) (t p2-at-jfk))
;;                   :effects '((t c1-at-jfk) (nil c1-in-p2)))
;;    (make-operator :name 'unload-c2-from-p1-at-sfo
;;                   :preconditions '((t c2-in-p1) (t p1-at-sfo))
;;                   :effects '((t c2-at-sfo) (nil c2-in-p1)))
;;    (make-operator :name 'unload-c2-from-p1-at-jfk
;;                   :preconditions '((t c2-in-p1) (t p1-at-jfk))
;;                   :effects '((t c2-at-jfk) (nil c2-in-p1)))
;;    (make-operator :name 'unload-c2-from-p2-at-sfo
;;                   :preconditions '((t c2-in-p2) (t p2-at-sfo))
;;                   :effects '((t c2-at-sfo) (nil c2-in-p2)))
;;    (make-operator :name 'unload-c2-from-p2-at-jfk
;;                   :preconditions '((t c2-in-p2) (t p2-at-jfk))
;;                   :effects '((t c2-at-jfk) (nil c2-in-p2)))

;;     ;; fly
;;    (make-operator :name 'fly-p1-from-sfo-to-jfk
;;                   :preconditions '((t p1-at-sfo))
;;                   :effects '((nil p1-at-sfo) (t p1-at-jfk)))
;;    (make-operator :name 'fly-p1-from-jfk-to-sfo
;;                   :preconditions '((t p1-at-jfk))
;;                   :effects '((nil p1-at-jfk) (t p1-at-sfo)))
;;    (make-operator :name 'fly-p2-from-sfo-to-jfk
;;                   :preconditions '((t p2-at-sfo))
;;                   :effects '((nil p2-at-sfo) (t p2-at-jfk)))
;;    (make-operator :name 'fly-p2-from-jfk-to-sfo
;;                   :preconditions '((t p2-at-jfk))
;;                   :effects '((nil p2-at-jfk) (t p2-at-sfo))))
;;   "A list of strips operators without their uniq gensyms set yet --
;; doesn't matter really -- but NOT including a goal or start operator")


;; ;;; cargo and planes at source
;; (defparameter *start-effects*
;;   '((t c1-at-sfo) (t c2-at-jfk) (t p1-at-sfo) (t p2-at-jfk)))

;; ;;; cargo at destination
;; (defparameter *goal-preconditions*	
;;   '((t c1-at-jfk) (t c2-at-sfo)))


;;;;; SOLUTION

;; * (time (do-pop))

;; Search Depth: 1
;; Search Depth: 2
;; Search Depth: 3
;; Search Depth: 4
;; Search Depth: 5
;; Search Depth: 6
;; Search Depth: 7
;; Search Depth: 8
;; Search Depth: 9
;; Search Depth: 10
;; Search Depth: 11
;; Search Depth: 12
;; Search Depth: 13
;; Solution Discovered:

;; Evaluation took:
;;   0.038 seconds of real time
;;   0.046875 seconds of total run time (0.046875 user, 0.000000 system)
;;   [ Run times consist of 0.015 seconds GC time, and 0.032 seconds non-GC time. ]
;;   123.68% CPU
;;   75,730,810 processor cycles
;;   27,983,600 bytes consed
  
;; #< PLAN operators: 
;; #S(OPERATOR
;;    :NAME LOAD-C1-TO-P1-AT-SFO
;;    :UNIQ G15266
;;    :PRECONDITIONS ((T C1-AT-SFO) (T P1-AT-SFO))
;;    :EFFECTS ((NIL C1-AT-SFO) (T C1-IN-P1)))
;; #S(OPERATOR
;;    :NAME LOAD-C2-TO-P1-AT-JFK
;;    :UNIQ G15265
;;    :PRECONDITIONS ((T C2-AT-JFK) (T P1-AT-JFK))
;;    :EFFECTS ((NIL C2-AT-JFK) (T C2-IN-P1)))
;; #S(OPERATOR
;;    :NAME UNLOAD-C1-FROM-P1-AT-JFK
;;    :UNIQ G15261
;;    :PRECONDITIONS ((T C1-IN-P1) (T P1-AT-JFK))
;;    :EFFECTS ((T C1-AT-JFK) (NIL C1-IN-P1)))
;; #S(OPERATOR
;;    :NAME FLY-P1-FROM-SFO-TO-JFK
;;    :UNIQ G15260
;;    :PRECONDITIONS ((T P1-AT-SFO))
;;    :EFFECTS ((NIL P1-AT-SFO) (T P1-AT-JFK)))
;; #S(OPERATOR
;;    :NAME FLY-P1-FROM-JFK-TO-SFO
;;    :UNIQ G15259
;;    :PRECONDITIONS ((T P1-AT-JFK))
;;    :EFFECTS ((NIL P1-AT-JFK) (T P1-AT-SFO)))
;; #S(OPERATOR
;;    :NAME UNLOAD-C2-FROM-P1-AT-SFO
;;    :UNIQ G15242
;;    :PRECONDITIONS ((T C2-IN-P1) (T P1-AT-SFO))
;;    :EFFECTS ((T C2-AT-SFO) (NIL C2-IN-P1)))
;; #S(OPERATOR
;;    :NAME START
;;    :UNIQ G14613
;;    :PRECONDITIONS NIL
;;    :EFFECTS ((T C1-AT-SFO) (T C2-AT-JFK) (T P1-AT-SFO) (T P2-AT-JFK)))
;; #S(OPERATOR
;;    :NAME GOAL
;;    :UNIQ G14614
;;    :PRECONDITIONS ((T C1-AT-JFK) (T C2-AT-SFO))
;;    :EFFECTS NIL) 
;; links: 
;; #< (G14613)START -> (G15266)LOAD-C1-TO-P1-AT-SFO : (T C1-AT-SFO) >
;; #< (G14613)START -> (G15265)LOAD-C2-TO-P1-AT-JFK : (T C2-AT-JFK) >
;; #< (G14613)START -> (G15266)LOAD-C1-TO-P1-AT-SFO : (T P1-AT-SFO) >
;; #< (G15266)LOAD-C1-TO-P1-AT-SFO -> (G15261)UNLOAD-C1-FROM-P1-AT-JFK : (T
;;                                                                        C1-IN-P1) >
;; #< (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G15265)LOAD-C2-TO-P1-AT-JFK : (T
;;                                                                      P1-AT-JFK) >
;; #< (G15265)LOAD-C2-TO-P1-AT-JFK -> (G15242)UNLOAD-C2-FROM-P1-AT-SFO : (T
;;                                                                        C2-IN-P1) >
;; #< (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G15261)UNLOAD-C1-FROM-P1-AT-JFK : (T
;;                                                                          P1-AT-JFK) >
;; #< (G15261)UNLOAD-C1-FROM-P1-AT-JFK -> (G14614)GOAL : (T C1-AT-JFK) >
;; #< (G14613)START -> (G15260)FLY-P1-FROM-SFO-TO-JFK : (T P1-AT-SFO) >
;; #< (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G15259)FLY-P1-FROM-JFK-TO-SFO : (T
;;                                                                        P1-AT-JFK) >
;; #< (G15259)FLY-P1-FROM-JFK-TO-SFO -> (G15242)UNLOAD-C2-FROM-P1-AT-SFO : (T
;;                                                                          P1-AT-SFO) >
;; #< (G15242)UNLOAD-C2-FROM-P1-AT-SFO -> (G14614)GOAL : (T C2-AT-SFO) > 
;; orderings: 
;; #[ (G15266)LOAD-C1-TO-P1-AT-SFO -> (G15260)FLY-P1-FROM-SFO-TO-JFK ]
;; #[ (G15266)LOAD-C1-TO-P1-AT-SFO -> (G14614)GOAL ]
;; #[ (G14613)START -> (G15266)LOAD-C1-TO-P1-AT-SFO ]
;; #[ (G15266)LOAD-C1-TO-P1-AT-SFO -> (G15261)UNLOAD-C1-FROM-P1-AT-JFK ]
;; #[ (G15265)LOAD-C2-TO-P1-AT-JFK -> (G15259)FLY-P1-FROM-JFK-TO-SFO ]
;; #[ (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G15265)LOAD-C2-TO-P1-AT-JFK ]
;; #[ (G15265)LOAD-C2-TO-P1-AT-JFK -> (G14614)GOAL ]
;; #[ (G14613)START -> (G15265)LOAD-C2-TO-P1-AT-JFK ]
;; #[ (G15265)LOAD-C2-TO-P1-AT-JFK -> (G15242)UNLOAD-C2-FROM-P1-AT-SFO ]
;; #[ (G15261)UNLOAD-C1-FROM-P1-AT-JFK -> (G15259)FLY-P1-FROM-JFK-TO-SFO ]
;; #[ (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G15261)UNLOAD-C1-FROM-P1-AT-JFK ]
;; #[ (G14613)START -> (G15261)UNLOAD-C1-FROM-P1-AT-JFK ]
;; #[ (G15261)UNLOAD-C1-FROM-P1-AT-JFK -> (G14614)GOAL ]
;; #[ (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G14614)GOAL ]
;; #[ (G14613)START -> (G15260)FLY-P1-FROM-SFO-TO-JFK ]
;; #[ (G15260)FLY-P1-FROM-SFO-TO-JFK -> (G15259)FLY-P1-FROM-JFK-TO-SFO ]
;; #[ (G15259)FLY-P1-FROM-JFK-TO-SFO -> (G14614)GOAL ]
;; #[ (G14613)START -> (G15259)FLY-P1-FROM-JFK-TO-SFO ]
;; #[ (G15259)FLY-P1-FROM-JFK-TO-SFO -> (G15242)UNLOAD-C2-FROM-P1-AT-SFO ]
;; #[ (G14613)START -> (G15242)UNLOAD-C2-FROM-P1-AT-SFO ]
;; #[ (G15242)UNLOAD-C2-FROM-P1-AT-SFO -> (G14614)GOAL ]
;; #[ (G14613)START -> (G14614)GOAL ]
;; >