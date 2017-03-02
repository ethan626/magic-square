;;;;;;;;;;;; Evolution Algorithm To Solve Magic Squares ;;;;;;;;;;;;;; 
;; (load "ethan.fas") 

(proclaim '(optimize-speed))

(defparameter *solutions* nil "List of all solutions we have found")

;;;;;;;;;;;;;;;;;;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro elements-not-in (square-size &rest sets) 
  "Returns a list of the numbers not in both squares"
  `(loop for i from 1 to (expt ,square-size 2) if (not-in-any? i ,@sets) collect i))

(defun square-compliment (set square-size)
  "Returns a list of all values which are not in the magic square"  
  (loop for i from 1 to (expt square-size 2) if (not-in? i set) collect i)) 

(defmacro elements-not-in (square-size &rest sets) 
  "Returns a list of the numbers not in both squares"
  `(loop for i from 1 to (expt ,square-size 2) if (not-in-any? i ,@sets) collect i))

;;;;;;;;;;;;;;;;;;;; Initial Generation ;;;;;;;;;;;;;;;;;;;;;;;

(defun make-generation (times-to-guess size-of-square) 
  "Returns a list of lists comprising the guesses of magic square solutions."
   
  (loop for i from 1 to times-to-guess collect
	
	(let ((num-list (loop for i from 1 to (expt size-of-square 2) collect i)))
	  
	  (loop for q from 1 to (expt size-of-square 2) collect 

		      (let* ((num-choice (random (length num-list)))
		      	     (choice (nth num-choice num-list)))
			
			(setf num-list (delete choice num-list))
			
			choice)))))

;;;;;;;;;;;;;;;;;; Fitness ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fitness (square-to-grade magic-num)
  "Returns a number representing the fitness of a square, with zero being a perfect. If zero fitness and a new solution the square-to-grade is added to *solutions*"
  
  (labels ((grade (square-to-grade)
		  "Returns a list of comprising lists of the row, column, and diagonal (num) totals for a given square."
		  (loop for funk in (list 'sum-rows 'sum-columns 'sum-diagonals) sum (funcall funk square-to-grade magic-num))))
	  
	  (let ((grade-of-square (grade square-to-grade)))

	    (if (and (= 0 grade-of-square) (not-in? square-to-grade *solutions*))
		(setf *solutions* (append  *solutions* (list square-to-grade))))
	    
	    grade-of-square)))	    

(defun sum-rows (square magic-num)
  "Sums the rows of a magic square guess, returns a number giving the sum"
  
  (let ((square-size (sqrt (length square))))
    
    (loop for i from 0 to (1- square-size) sum
	  (abs (- magic-num (loop for q from 0 to (1- square-size) sum (nth (+ q (* i square-size)) square)))))))

(defun sum-columns (square magic-num)
  "Sums the columns of a magic square guess"
  
  (let ((square-size (sqrt (length square)))
	(indx-square (indx square)))

    (loop for i from 0 to (1- square-size) sum
	 (abs (- magic-num (loop for q from 0 to indx-square by square-size sum (nth (+ q i) square)))))))

(defun sum-diagonals (square magic-num)
  "Sums the diagonals of a magic square guess, returns a number of the combined (magic-num - sum)"
  
  (let ((square-size (sqrt (length square))))
   
    (+ (abs (- magic-num (loop for i from (1- square-size) to (- (indx square) (1- square-size)) by (1- square-size) sum (nth i square))))
       (abs (- magic-num (loop for i from 0 to (indx square) by  (1+ square-size) sum (nth i square)))))))

;;;;;;;;;;;;;;;; Breeding ;;;;;;;;;;;;;;;;;;;;;;

(defun cross-breed (square-size num-of-crosses p1 p2) 
  "Cross breeds two parents and returns a single child. Parent2 has the weaker genes"
  
  (let* ((new-chrome)				     
	 (step-size (floor (/ (expt square-size 2) num-of-crosses)))
	 (gene)
	 (doubles)
	 (start 0)
	 (end step-size)
	 (parent p1)    
    	 (rpl-list)
	 (square-len (expt square-size 2)))
    
    (labels ((replace-doubles (chrome acc) 
			      "Returns a list comprising the values of chrome with the values which are in doubles replaced with values which are not in rpl-list"
			      
			      (cond ((null chrome) acc)
				    
				    ((in? (car chrome) doubles)
				     
				     (replace-doubles (cdr chrome) (append acc (list (pop rpl-list)))))
				    
				    (t (replace-doubles (cdr chrome) (append acc (list (car chrome)))))))
	     
	     (use-other-parent ()
			       "Switches which parent we are using"
			       (if (eq parent p1) (setf parent p2) (setf parent p1)))

	     (make-chrome ()
			  "Makes the new chromisome by taking sections from the parents"

			  (loop for i from 1 to num-of-crosses do 
				
				(if (= num-of-crosses i)(setf end square-len))
				
				(setf doubles (append doubles (intersection gene new-chrome)))
				(setf gene (subseq parent start end))
				(setf rpl-list (elements-not-in square-size new-chrome gene))
				(setf gene (replace-doubles gene nil)  new-chrome (append new-chrome gene))
				(incf start step-size)
				(incf end step-size)
				
				(use-other-parent))
			  
			  (when (or (> end square-len) (<= (- square-len end) step-size)) (setf end square-len))

			  new-chrome))
	    
	    (make-chrome))))

(defun sort-by-fitness (generation magic-num square-size)
  "Returns a sorted list of the generation sorted by fitness with the fittest at the head of the list"
  
  (labels ((add-fitness-to-chrome (chrome)
	  "Adds the fitness of a chromisome to the end of the chromisome"
		  (append chrome (list (fitness chrome magic-num))))
	    
	   (generation-with-fitness ()
				    (mapcar #'add-fitness-to-chrome generation)))

	  (mapcar #'(lambda(x) (subseq x 0 (expt square-size 2))) (sort (generation-with-fitness)  #'<last))))

;;;;;;;;;;;;;;;;;;;;;;;;; Master Loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main-loop (gen square-size)
  "Master loop of the algorithm"

  (let ((generation gen)
	(magic-num (/ (* square-size (1+ (expt square-size 2))) 2)))
    
    (loop until (>= (length *solutions*) 4) do
	  
	  
	  (setf generation (sort-by-fitness generation magic-num square-size))
	  (print generation)
	  (print *solutions*)

	  (setf generation (append
			    
		     (loop for i from 0 to (* (/ 3 10) (length gen)) collect
		     	   (cross-breed square-size square-size (nth i generation) (nth (1+ i) generation)))
		     
		     (loop for i from 0 to (* (/ 3 10) (length gen)) collect
		     	   (cross-breed square-size 2 (nth (1+ i) generation) (nth i generation)))

		     (loop for i from 0 to (* (/ 3 10) (length gen)) collect
		     	   (cross-breed square-size 6 (nth (1+ i) generation) (nth i generation)))					
	     
		     (make-generation (* (/ 1 10) (length gen)) square-size))))
    
    *solutions*))

(main-loop (make-generation 4000 3) 3)

;;; Parses command line args and starts the program 
;; (let ((size-of-square (parse-integer (nth 0 *args*)))
;;       (generation-size (parse-integer (nth 1 *args*))))
  
;;  (main-loop (make-generation generation-size size-of-square) size-of-square))





 
