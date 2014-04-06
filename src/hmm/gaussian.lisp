;;;; Author: Benjamin E. Lambert (ben@benjaminlambert)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Gaussian, Gaussian mixtures, and related distance/probability computations.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Definitions of distributions  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Definitions of distributions")

(defclass* distribution ()
  ((dimensions 0 ira :type fixnum)) ;; this is just in case we want to run a sanity check
  (:automatic-accessors t)
  (:automatic-initargs t)
  (:name-prefix "distribution" "-")
  (:documentation "Represents a single distribution of any kind..."))

(defclass* gaussian (distribution)
  ((mean #() ira :type (simple-array single-float))
   (variance #() ira :type (simple-array (single-float 0.0 *) *))
   (dimensions 0 ira :type fixnum) ;; this is just in case we want to run a sanity check
   (constant1 nil ra :type (or null single-float))
   (constant2 nil ra :type (or null single-float)))
  (:automatic-accessors t)
  (:automatic-initargs t)
  (:name-prefix "gaussian" "-")
  (:documentation "Represents a single Gaussian distribution."))

(defmethod initialize-instance :after ((g gaussian) &key)
  (declare (optimize (speed 3)))
  (assert (= (length (the simple-array (gaussian-mean g))) (length (the simple-array (gaussian-variance g)))))
  (unless (= (the fixnum (gaussian-dimensions g)) (length (the simple-array (gaussian-mean g))))
    (setf (gaussian-dimensions g) (length (the simple-array (gaussian-mean g)))))
  (let (;;(const1 (- (* 0.5 (the single-float (vector-sum (vector-log (gaussian-variance g)))))))
	(const1 (- (* 0.5 (the single-float (vector-log-sum (gaussian-variance g))))))
	(const2 (- (* 0.5 (the fixnum (gaussian-dimensions g)) (log (* 2 pi))))))
    (setf (gaussian-constant1 g) (coerce const1 'single-float))
    (setf (gaussian-constant2 g) (coerce const2 'single-float))
    ;; (setf (gaussian-constant1 g) const1)
    ;; (setf (gaussian-constant2 g) const2)
    ))

(defclass* gaussian-mixture (distribution)
  ((gaussians #() :type (simple-array gaussian))
   (weights #() ira :type (simple-array single-float))
   (dimensions 0 ira :type fixnum)  ;; this is the feature count
   (components 0 ira :type fixnum)) ;; this tells us how many gaussians are in the mixture
  (:automatic-accessors t)
  (:automatic-initargs t)
  (:name-prefix "gaussian-mixture" "-")
  (:documentation   "Represents a single Gaussian mixture distribution."))

(defun make-gaussian-mixture (dimensions components)
  "Convenience function (?) for creating Gaussian mixutre models."
  (let ((gm (make-instance 'gaussian-mixture :dimensions dimensions :components components)))
    ;;(setf (gaussian-mixture-gaussians gm) (make-array components :element-type 'gaussian :initial-element (make-instance 'gaussian)))
    (setf (gaussian-mixture-gaussians gm) (make-array components :element-type '(or gaussian null) :initial-element nil))
    (setf (gaussian-mixture-weights gm) (make-array components :element-type 'single-float :initial-element 0.0))
    gm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Aux ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Aux")

(defconstant +pi+ (coerce pi 'single-float)
  "A single precision version of the constant 'pi'.")

(defconstant +log-two-pi+ (coerce (log (* 2.0 pi)) 'single-float)
  "A single precision version of the constant 'pi'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Computing Gaussian distance/probability ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Gaussian distance/probability generics")

;; TODO - There's some redundancy here... we don't need separate generic functions for the mixture probabilities...!!

(defgeneric gaussian-distance (x distribution)
  (:documentation "Compute the 'distance' of observation x from the given distribution.  This 'distance' is used by the other functions here."))

(defgeneric gaussian-probability (x distribution)
  (:documentation "Probability of X according to the given distribution.  (We don't use this much because the probabilities are very small, and so we use log probabilities mostly."))

(defgeneric log-gaussian-probability (x distribution)
  (:documentation "Log probability of X according to the given distribution."))

(defgeneric negative-log-gaussian-probability (x distribution)
  (:documentation "*Negative* log probability of X according to the given distribution.  This is ever so slightly easier to get than the log probability, so we have them separate."))

(defgeneric lgk (x gaussian-mixture-model k)
  (:documentation "This is the log probability of X according to gaussian #k of the mixture (?)"))

(defgeneric log-gaussian-mixture-probability (x gaussian-mixture)
  (:documentation "Log probability of X according to the given distribution."))

(defgeneric log-gaussian-mixture-probability-approx (x gaussian-mixture &key)
  (:documentation "An approximation of the true mixture probabilty, using only the top n matching gaussians in the mixture."))

(defgeneric log-gaussian-mixture-probability-rough (x gaussian-mixture)
  (:documentation "This approximates the log probability of x in the given GMM, by taking only the biggest match
   of all the gaussians in the mixture."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Computing Gaussian distance/probability -- Implemented ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Gaussian distance/probability - Implemented functions")

(cl-user::subsection "Gaussian style distances")

(defmethod gaussian-distance (x (distribution gaussian))
  "Compute Gaussian 'distance' (?).  This is just the exponent part...?
   This is:
     Σ_d  (x - mu)² / sigma²
   summed over all the dimensions.
   This is a *distance*, not a *score*."
  (declare (optimize (speed 3)))
  (declare ((simple-array single-float) x) (gaussian distribution))
  (let ((cumulative-probability 0.0)
	(mean (gaussian-mean distribution))
	(variance (gaussian-variance distribution)))
    (declare ((simple-array single-float) mean variance) (single-float cumulative-probability))
    (dotimes (i (the fixnum (gaussian-dimensions distribution)))
      (let ((xi (elt x i))
	    (mu-i (elt mean i))
	    (sigma-i (elt variance i)))
	(declare (single-float xi mu-i sigma-i))
	;; Getting a div-by-zero here:
	;;(incf cumulative-probability (/ (expt (- xi mu-i) 2) sigma-i))
	(unless (= sigma-i 0.0)
	  (incf cumulative-probability (/ (expt (- xi mu-i) 2) sigma-i)))))
    cumulative-probability))

(defmethod lgk (x (gmm gaussian-mixture) k)
  "This is the log probability of X according to gaussian #k of the mixture (?)"
  ;;(declare (optimize (speed 3)))
  (let* ((gaussian (aref (the (simple-array gaussian) (gaussian-mixture-gaussians gmm)) k))
	 (dist (gaussian-distance x gaussian))
	 (term1 (* -0.5 dist))
	 (wk (aref (the (simple-array single-float) (gaussian-mixture-weights gmm)) k))
	 ;;(lgk (+ term1 (log ck) (log wk))))
	 (lgk (+ (the single-float (log wk))
		 ;;(- (* 0.5 (gaussian-dimensions gaussian) (log (* 2 pi)))) ;; optional
		 ;;(- (* 0.5 (vector-sum (vector-log (gaussian-variance gaussian))))) ;; pre-computed.
		 (the single-float (gaussian-constant1 gaussian))
		 (the single-float (gaussian-constant2 gaussian))
		 term1)))
    (declare (single-float dist))
    (setf lgk (coerce lgk 'single-float))
    (if (sb-ext:float-nan-p lgk)
	0.0
	lgk)))

(cl-user::subsection "True Gaussian probabilities")

(defmethod gaussian-probability (x (g gaussian))
  "Compute the true gaussian probability of X according to the given Gaussian distribution."
  (let* ((exponent (gaussian-distance x g))
	 (k (gaussian-dimensions g))
	 (non-normalized-probability (exp (- (/ exponent 2))))
	 (variance-norm (vector-norm (gaussian-variance g)))
	 (normalization-constant (* (expt (* 2 +pi+) (- (/ k 2))) (expt variance-norm -0.5)))
	 (probability (/ non-normalized-probability normalization-constant)))
    probability))

(defmethod gaussian-probability (x (gmm gaussian-mixture))
  "Compute the true gaussian probability of X according to the given Gaussian mixture distribution."
  (let ((total-prob 0.0))
    (dotimes (k (gaussian-mixture-components gmm))
      (let ((this-lgk (lgk x gmm k)))
	(incf total-prob (exp this-lgk))))
    total-prob))


(cl-user::subsection "Log Gaussian probabilities")

(defmethod negative-log-gaussian-probability (x (g gaussian))
  "Compute the negative log probability of a data point in the given distribution.
   This is the actual negative log probability, unlike the gaussian 'distance'.  

   This includes the 'first term' in the equation, unlike the distance.  This is computed as:
     0.5 * (log(2 * pi * sigma^2) + ((x - mu) ^2) / sigma^2)
   The log probability would also have a negation in front of it, but since this is a negative log probability
   that negation is cancelled out."
  (declare (optimize (speed 3)))
  (let* ((gaussian-distance (gaussian-distance x g))
	 (term1 0.0))
    (declare (single-float term1 gaussian-distance))
    (loop for i below (the fixnum (gaussian-dimensions g))
       for variance of-type (single-float 1e-30 *) = (aref (the (simple-array single-float) (gaussian-variance g)) i)
       for log-variance of-type single-float = (log (the (single-float 1e-30 *) variance))
       for increment = (+ log-variance (the single-float +log-two-pi+)) do
	 (incf term1 increment))
    ;; This adds them and divides by two..
    (coerce (* 0.5 (+ term1 gaussian-distance)) 'single-float)))

(defmethod negative-log-gaussian-probability (x (gmm gaussian-mixture))
  "Negative log gaussian probability of a mixture is simply the additive inverse of the log gaussian probability."
  (declare (optimize (speed 3)))
  (- (the single-float (log-gaussian-probability x gmm))))

(defmethod log-gaussian-probability (x (g distribution))
  "For an arbitrary distrinbution, define the log gaussian probability simply as the additive inverse
   of the 'negative-log-gaussian-probability'."
  (declare (optimize (speed 3)))
  (- (the single-float (negative-log-gaussian-probability x g))))

;; THIS ONE DOESN'T SEEM RIGHT!!!
(defmethod log-gaussian-probability (x (gmm gaussian-mixture))
  "For a gaussian mixture distrinbution, define the log gaussian probability as the log
   of the true probability... THIS DOESN'T SEEM RIGHT..."
  (let* ((prob (gaussian-probability x gmm))
	 (log-prob (log prob)))
    (declare (single-float prob))
    (the single-float log-prob)))

(defparameter *lgk-array* (make-array 32 :element-type 'single-float :initial-element 0.0)
  "We need to compute an array of ~32 lgk values for every single frame x every gaussian mixture.
   It would be hugely expensive in terms of consing to keep allocating this array again and again, so
   we keep around one, single array and just reuse it.")

(declaim ((simple-array single-float) *lgk-array*))

(defun get-max-lgk (x gmm)
  "Compute all the lgk values, and return the maximum value and an array of all the lgk values (so we don't have to
   recompute them all)."
  (declare (optimize (speed 3)))
  ;; Make sure we have an array to store the results in
  (when (/= (length *lgk-array*) (the fixnum (gaussian-mixture-components gmm)))
    (setf *lgk-array* (make-array (the fixnum (gaussian-mixture-components gmm)) :element-type 'single-float :initial-element 0.0)))
  ;; This is a lot of the consing right here...
  (let* ((lgk-values *lgk-array*)
	 (max-lgk most-negative-single-float))
    ;; Get the max lgk
    (loop for k from 0 below (the fixnum (gaussian-mixture-components gmm))
       for lgk = (lgk x gmm k) do
	 (setf (aref lgk-values k) lgk)
	 (when (> lgk max-lgk)
	   (setf max-lgk lgk)))
    (values max-lgk lgk-values)))

(defmethod log-gaussian-mixture-probability (x (gmm gaussian-mixture))
  "This computes the exact(?) log probabilty of observation x, in the given
   gaussian mixture model.
   THIS IS THE FUNCTION WE'RE USING"
  ;;(declare (optimize (speed 3)))
  (multiple-value-bind (max-lgk lgk-values)
      (get-max-lgk x gmm)
    (declare (single-float max-lgk) ((simple-array single-float) lgk-values))
    (let ((sum 0.0))
      (declare ((single-float 0.0 *) sum))
      (loop for k from 0 below (the fixnum (gaussian-mixture-components gmm))
	 for lgk = (aref lgk-values k) 
	 for diff = (- lgk max-lgk) do
	   (incf sum (exp diff)))
      (+ max-lgk (the single-float (log sum))))))

(defmethod log-gaussian-mixture-probability-approx (x (gmm gaussian-mixture) &key (top-n 4))
  "An approximation of the true mixture probabilty, using only the top n matching gaussians in the mixture."
  (declare ;;(optimize (speed 3))
	   (fixnum top-n))
  (multiple-value-bind (max-lgk lgk-values)
      (get-max-lgk x gmm)
    (declare (single-float max-lgk) ((simple-array single-float) lgk-values))
    (setf lgk-values (sort lgk-values '>))
    (let ((sum 0.0))
      (declare ((single-float 0.0 *) sum))
      (loop for k from 0 below top-n
	 for lgk = (aref lgk-values k)
	 for diff = (- lgk max-lgk) do
	   (incf sum (exp diff)))
      (+ max-lgk (the single-float (log sum))))))

(defmethod log-gaussian-mixture-probability-rough (x (gmm gaussian-mixture))
  "This approximates the log probability of x in the given GMM, by taking only the biggest match
   of all the gaussians in the mixture."
  (get-max-lgk x gmm))

