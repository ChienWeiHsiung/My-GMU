(defun difference (list1 list2 &key (measure 'count))
  (let ((dif 0))
    (when (equalp measure 'count) ;;; count 
      (dolist (i (mapcar (lambda (x y)(equalp x y)) list1 list2))
	(if (equalp i 'NIL)
	    (incf dif 1))))      
    (when (equalp measure 'squared) ;;; squared
      (dolist (i (mapcar (lambda (x y)(- x y)) list1 list2))
	(incf dif (* i i))))
    (when (equalp measure 'manhattan) ;;;manhattan
      (dolist (i (mapcar (lambda (x y)(abs (- x y))) list1 list2))
	(incf dif i)))
    ;;;retrun difference
    dif)
  )  

(defun k-nearest-neighbor (examples new-example &key (k 1) (measure 'count))
   ;;;compute difference of each example and list them with their classes
  (let ((dif-list (mapcar (lambda (x)(list (first (cdr x)) (difference (first x) (first new-example) :measure measure))) examples)))
    ;;;sort from small to large & subsequence(get first k smallest difference with their classes from examples)
    (setf dif-list (subseq (sort dif-list #'< :key #'second) 0 k))
    ;;;extract the classes from list
    (setf dif-list (mapcar (lambda (x)(first (first x))) dif-list))
    ;;;count the frequency of a class 
    (setf dif-list (mapcar (lambda (x) (list x (count x dif-list))) (remove-duplicates dif-list))) 
    ;;;return the most common item(class)
    (first (first (sort dif-list #'> :key #'second))))
  )

(defun generalization (training-examples test-examples &key (k 1) (measure 'count))
  (let ((accuracy 0))
    ;;run every test-examples
    (dolist (test test-examples)
      ;;;run k-nearest-neighbor and compare its prediction to original class
      (if (equalp (first (first (cdr test))) (k-nearest-neighbor training-examples test :k k :measure measure))
	  (incf accuracy 1)))
    ;;compute accuracy
    (float (/ accuracy (length test-examples)))))
  )

;;;Question answer :
;;;(k-nearest-neighbor *voting-records-short* *neutral* :k 3 :measure 'squared)
;;; A : 0.9
;;;(generalization (butlast *voting-records-short* 94) (last *voting-records-short* 94) :measure 'squared :k 3)
;;; A : 0.9255319

;;;Approach :
;;;1. function difference :
;;;  step 1 : Use ¡§when¡¨ to test for what type the measure is. Then, use "mapcar" and "lambda" to compute the data and get a list of the results.
;;;  step 2 : Use dolist to add the results to the local variable dif. Fianlly, return dif.
;;;2. function k-nearest-neighbor :
;;;  step 1 : Use "mapcar" and "lambda" to compute difference of each example , and then list them with their classes.
;;;           Ex: ((((0.9) 6.199999) ((0.9) 7.199999) ((0.1) 6.5999994) ((0.9) 7.399999).....))
;;;  step 2 : Sort them from small to large, and then use subseq to get first k smallest difference with their classes from examples.
;;;  step 3 : Use "mapcar" and "lambda" to extract the classes from list and then list them. Ex :((0.9) (0.1) (0.9)) -> (0.9 0.1 0.9)
;;;  step 4 : Use "mapcar" and "lambda" to count the frequency of a class and list them with their classes. Ex :  ((0.1 1) (0.9 2))
;;;  step 5 : Return the most common class(prediction).
;;;3. function generalization :
;;;  step 1 : Use dolist to run through every test-examples.
;;;  step 2 : Run k-nearest-neighbor for each test-examples with training-examples and compare its prediction to original class. If they¡¦re equal, increase accuracy by 1.
;;;  step 3 : Compute accuracy and then return in form of percentage.

;;;Learned :
;;;Although I have done the assignment for practice, I was still unfamiliar with the structure and grammar of lisp while doing this homework. 
;;;For example, in C and Java, function print is like ¡§printf()¡¨ and ¡§print()¡¨. But in lisp, all the things have to be within parentheses. That caused a lot of troubles while I was coding.
;;;When I wanted to compile the function, the error about parentheses came out often and it took  time to look for where I missed a right or left parenthesis.
;;;Also, Emacs won¡¦t tell really details about where the error is and what you should do to fix it.
;;;Some IDEs for Java and python, like Spyder, will give suggestions when typing a function. 
;;;For example, type "list.". It will give a list of functions you can choose and what parameter they need. I can easily choose what I want and look for what I can also use.
;;;Also, they will automatically add ¡§)¡¨, ¡§}¡¨ for you and tell me what the error is and what should I do to fix it in detail.
;;;However, Emacs won¡¦t. So I spent lots of time searching for the details about formats of functions on the internet, such as return value, parameter.
;;;I also tried them before I actually put them into my function. Test what will I get after calling this function and how to combine with other functions.
;;;Now, these functions used in my code are deeply in my brain.
;;;Therefore, after completing the first function with lots of troubles, I got more familiar with lisp and finished the rest of functions pretty fast.
;;;This homework is quite appropriate as the first project because it not only gives me a chance to learn more about lisp, but also helps me link lisp to the course content.
;;;Or I felt I just learned a new programming language called Lisp and a new kind of knowledge called Artificial Intelligence.
;;;I¡¦m going to practice more lisp to get ready for upcoming project and the extra credit project : Decision tree now.



