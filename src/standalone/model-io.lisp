;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Input and output of model files")

(cl-user::todo "Large portions of this file are probably largely obsolete at this point.")

(cl-user::section "Data strutures and function for loading and saving Gaussians to files.")

(defstruct disk-gaussian
  "Less strictly typed version of gaussian.  More suitable to read/write from disk."
  (mean #() :type vector)
  (variance #() :type vector)
  (dimensions 0 :type number))

(defun gaussian->disk-gaussian (g)
  "Convert an in-memory Gaussian to a disk-based Gaussian."
  (make-disk-gaussian :mean (gaussian-mean g)
		      :variance (gaussian-variance g)
		      :dimensions (gaussian-dimensions g)))

(defun disk-gaussian->gaussian (g)
  "Convert a disk-based Gaussian to an in-memory Gaussian."
  (create-gaussian (disk-gaussian-mean g) (disk-gaussian-variance g)))

(defun create-gaussian (mean variance)
  "Given a mean and a variance vector, create a Gaussian structure.
   Converts vectors to efficient data structure types."
  (assert (= (length mean) (length variance)))
  (let ((dimensions (length mean)))
    (make-instance 'gaussian 
		   :mean (coerce mean '(vector single-float))
		   :variance (coerce variance '(vector (single-float 0.0 *)))
		   :dimensions dimensions)))

(defun save-gaussian (g filename)
  "Save a Gaussian to a file.  First converts the type to a disk-based gaussian."
  (save-object (gaussian->disk-gaussian g) filename)
  filename)

(defun load-gaussian (filename)
  "Load a gaussian from a file."
  (disk-gaussian->gaussian (load-object filename)))
 

(cl-user::section "Data strutures and function for loading and saving word/phoneme HMMs to files.")


(defstruct disk-hmm
  "The disk version of an HMM.  Contains different structure types."
  (word nil :type string)
  (state-count 0 :type fixnum)
  (emission-distributions nil :type (simple-array disk-gaussian))
  (transition-probabilities nil :type simple-array))

(defun hmm->disk-hmm (hmm)
  "Convert and HMM to a disk HMM."
  (make-disk-hmm :word (hmm-word hmm)
		 :state-count (hmm-state-count hmm)
		 :emission-distributions (map '(vector disk-gaussian *) 'gaussian->disk-gaussian (hmm-emission-distributions hmm) )
		 :transition-probabilities (hmm-transition-probabilities hmm)))

(defun disk-hmm->hmm (hmm)
  "Convert a disk HMM to a regular HMM."
  (create-hmm (disk-hmm-word hmm)
	      (disk-hmm-state-count hmm)
	      (map '(vector gaussian) 'disk-gaussian->gaussian (disk-hmm-emission-distributions hmm))
	      (disk-hmm-transition-probabilities hmm)))

(defun create-hmm (word state-count emission-dist transition-probs)
  "Create an HMM from the basic pieces:
         -the word/phoneme,
         -the state count
         -the list of emission Gaussians
         -a transition probability table."
  (assert (= (length emission-dist) state-count))
  (make-instance 'hmm 
		 :word word
		 :state-count state-count
		 :emission-distributions (coerce emission-dist '(vector gaussian))
		 :transition-probabilities (coerce-2d-array transition-probs 'single-float)))

(defun coerce-2d-array (array element-type)
  "Coerce a 2D array to the specified type.  There is no built-in function for this??"
  (let ((new-array (make-array (array-dimensions array) :element-type element-type)))
    (dotimes (x (array-dimension array 0))
      (dotimes (y (array-dimension array 1))
	(setf (aref new-array x y)
	      (aref array x y))))
    new-array))

(defun save-hmm (hmm filename)
  "Save an HMM to a file."
  (save-object (hmm->disk-hmm hmm) filename)
  filename)

(defun load-hmm (filename)
  "Load an HMM from a file."
  (let* ((disk-hmm (load-object filename))
	 (hmm (disk-hmm->hmm disk-hmm)))
    (dotimes (i (hmm-state-count hmm))
      (push (hmm-word hmm)
	    (hmm-state-ids hmm)))
    hmm))
