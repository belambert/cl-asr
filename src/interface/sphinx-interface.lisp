;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "General functions for reading entire S3 acoustic model files.")


;; Don't need a READ-DICTIONARY FUNCTION, WE ALREADY HAVE ONE IN dictionary.lisp
;;(defun read-dictionary (filename &key (force nil))
;; Ditto...?
;; (defun read-noise-dictionary (filename)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load model ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Load model")

(defun load-s3-gmms (&key means variances mixture-weights)
  "Load the Sphinx3 acoustic model Gaussian mixture models."
  (let* ((means (read-gaussian means))
	 (variances (read-gaussian variances))
	 (mixw (read-mixture-weights mixture-weights))
	 (state-count (length means))
	 (density (length (first means)))
	 (feature-count (length (first (first means))))
	 (gmms (make-array state-count)))
    (loop for i from 0 below state-count
       for gmm = (make-gaussian-mixture feature-count density)
       for this-means in means
       for this-variances in variances
       for this-mixw in mixw do
	 (loop for mean in this-means
	    for j from 0 below density
	    for variance in this-variances
	    for gaussian = (make-instance 'gaussian :mean mean :variance variance :dimensions feature-count) do
	      (setf (aref (gaussian-mixture-gaussians gmm) j) gaussian))
	 (setf (gaussian-mixture-weights gmm) this-mixw)
	 (setf (aref gmms i) gmm))
    gmms))

(defun get-tmat-table (triphones tmats)
  "Create a hash table mapping from triphones to transition matrices."
  (let ((table (make-hash-table :test 'equalp)))
    (loop for phone in triphones
	 for tmat in tmats do
	 (setf (gethash (triphone-base phone) table) tmat))
    table))

(defstruct (acoustic-model (:print-function (lambda (struct stream depth)
					      (declare (ignore depth))
					      (print-unreadable-object (struct stream)
						(format stream "ACOUSTIC-MODEL #SENONES=~:D #TMATS=~:D #TRIPHONES=~:D LDA-DIM=~Dx~D"
							(length (acoustic-model-gmms struct))
							(length (acoustic-model-tmats struct))
							(length (acoustic-model-triphones struct))
							(if (acoustic-model-transformation-matrix struct)
							    (array-dimension (acoustic-model-transformation-matrix struct) 0) 0)
							(if (acoustic-model-transformation-matrix struct)
							    (array-dimension (acoustic-model-transformation-matrix struct) 1) 0))))))
  "Represents an acoustic model in memory."
  transformation-matrix   ;; lda transformation matrix
  gmms                    ;; an array of the gmms
  tmats                   ;; an array of the transition matricies
  triphones               ;; a sequence containing a struct for each triphone in the model
  triphone-table)

(defun triphone-hash-key (triphone)
  (list (triphone-base triphone)
	(triphone-left triphone)
	(triphone-right triphone)
	(triphone-position triphone)))

(defun triphones->triphone-table (triphones)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for triphone in triphones do
	 (assert (not (gethash (triphone-hash-key triphone) table)))
	 (push triphone (gethash (triphone-hash-key triphone) table))
	 (when (triphone-right triphone)
	   (push triphone (gethash (list (triphone-base triphone) nil (triphone-right triphone) (triphone-position triphone)) table)))
	 (when (triphone-left triphone)
	   (push triphone (gethash (list (triphone-base triphone) (triphone-left triphone) nil (triphone-position triphone)) table))))
    table))

(defun load-s3-model (&key folder mdef mean var mixw tmat lda dict)
  "Load a Sphinx3 acoustic model into memory."
  (when dict (read-dictionary dict))
  (when folder (setf folder (cl-fad:pathname-as-directory folder)))
  (unless mdef (setf mdef (merge-pathnames folder "mdef")))
  (unless mean (setf mean (merge-pathnames folder "mean")))
  (unless var (setf var (merge-pathnames folder "var")))
  (unless mixw (setf mixw (merge-pathnames folder "mixw")))
  (unless tmat (setf tmat (merge-pathnames folder "tmat")))
  (when (and (not lda) (probe-file (merge-pathnames folder "lda")))
    (setf lda (merge-pathnames folder "lda")))
  (when (and (not lda) (probe-file (merge-pathnames folder "feature_transform")))
    (setf lda (merge-pathnames folder "feature_transform")))


  (let* ((gmms (load-s3-gmms :means mean :variances var :mixture-weights mixw))
	 (tmats (coerce (read-transition-matrices tmat) 'vector))
	 (triphones (read-mdef mdef))
	 (triphone-table (triphones->triphone-table triphones))
	 (transformation-matrix (when lda (read-feature-transform lda)))
	 (model (make-acoustic-model :transformation-matrix transformation-matrix
				     :gmms gmms
				     :tmats tmats
				     :triphones triphones
				     :triphone-table triphone-table)))
    (setf *acoustic-model* model)
    ;; After we're done, do a full GC since we created a lot of garbage in the process of reading the acoustic model
    (sb-ext:gc :full t)
    model))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read a segment definition file 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Read a segment definition file")

;; Something like this...?
(defun read-segment-def-file (filename)
  "Read a 'segment definition file'.  This file shows where audio segments begin/end within a longer audio file."
  (declare (ignore filename))
  ;;a960521 100028 100288 a960521_100028_100288_Greg_Williams_Male_Native_Spontaneous_Medium_Clean_Off
  ;;a960521 10015 10408 a960521_10015_10408_Ted_Koppel_Male_Native_Planned_High_Clean_Off
  ;;a960521 100286 100522 a960521_100286_100522_Greg_Williams_Male_Native_Spontaneous_Medium_Clean_Off
  ;;a960521 100520 100659 a960521_100520_100659_Greg_Williams_Male_Native_Spontaneous_Medium_Clean_Off
  ())


