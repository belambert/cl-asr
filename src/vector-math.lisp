;; Copyright 2010-2018 Ben Lambert

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Vector operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vector-norm (array)
  "Given an array of numbers, compute the 'norm': i.e. the sum of sqaures."
  (let ((norm 0.0))
    (declare (single-float norm))
    (loop for x across array do
	 (incf norm (expt x 2)))
    norm))

(defun vector-sum (seq)
  "Sum all the elements of the given seq."
  (declare (optimize (speed 3))
	   (simple-array seq))
  (reduce (lambda (x y) (declare (single-float x y)) (+ x y)) seq))

(defun safe-log (x)
  (declare ((single-float 0.0 *) x))
  (if (/= x 0)
      (log x)
      x))

(defun vector-log (seq)
  "Return a vector that contains the log of each element of seq."
  (declare (optimize (speed 3))
	   (simple-array seq))
  (map 'vector 'safe-log seq))

(defun vector-log-sum (seq)
  "Return a vector that contains the log of each element of seq."
  (declare (optimize (speed 3))
	   (simple-array seq))
  (reduce (lambda (x y) (declare (single-float x y)) (+ x y)) seq :key 'safe-log))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Matrix operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matrix-multiply (A B)
  "Compute the matrix product of two 2d arrays.
   Taken from: http://rosettacode.org/wiki/Matrix_multiplication#Common_Lisp"
  (let* ((m (elt (array-dimensions A) 0))
	 (n (elt (array-dimensions A) 1))
         (l (elt (array-dimensions B) 1))
         (C (make-array `(,m ,l) :initial-element 0)))
    ;; This causes failure on 1d arrays..?
    ;;(assert (= (array-dimension B 0) (array-dimension A 1)))
    (loop for i from 0 to (- m 1) do
              (loop for k from 0 to (- l 1) do
                    (setf (aref C i k)
                          (loop for j from 0 to (- n 1)
                                sum (* (aref A i j)
                                       (aref B j k))))))
    C))

(defun matrix-multiply-1d (vector array)
  "Matrix multiplication where one of the two matrices is a '1d' array."
  (let* ((vector-length (length vector))
	 (array-width (array-dimension array 0))
	 (array-height (array-dimension array 1))
	 (result (make-array array-width :initial-element 0.0 :element-type 'single-float)))
    (assert (= vector-length array-height))
    (loop for i from 0 below array-width do
	 (let ((sum 0.0))
	   (loop for j from 0 below vector-length do
		(incf sum (* (aref vector j) (aref array i j))))
	   (setf (aref result i) sum)))
    result))

(defun matrix-transpose (A)
  "Compute the transpose of a matrix from a 2d Lisp array (i.e. turn it on its side)."
  (let* ((m (first (array-dimensions A)))
         (n (second (array-dimensions A)))
         (B (make-array `(,n ,m) :initial-element 0)))
    (loop for i from 0 to (- m 1) do
          (loop for j from 0 to (- n 1) do
                (setf (aref B j i)
                      (aref A i j))))
    B))

(defun 2d-array->array-of-arrays (2d-array)
  "Create an array of arrays from a 2d array."
  (let* ((vector-count (array-dimension 2d-array 0))
	 (vector-length (array-dimension 2d-array 1))
	 (array (make-array vector-count)))
    (loop for vector-number from 0 below vector-count
       for vector = (make-array vector-length :element-type 'single-float) do
	 (setf (aref array vector-number) vector)
	 (loop for i from 0 below vector-length do
	      (setf (aref vector i) (aref 2d-array vector-number i))))
    array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Simple vector arithmetic -- Used for training? ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vector-sequence-add (result-type sequence-of-vectors)
  "Given a seqence of 'vectors', perform vector addition and return the sum in the
   specified sequence type.
   Simpler/slower version of this would be to use array-operations::array+
   But, this is a specialized use, so it maybe better to stick with this custom version..."
  (unless sequence-of-vectors
    (return-from vector-sequence-add sequence-of-vectors))
  (let* ((dimensions (length (elt sequence-of-vectors 0)))
	 (vector-count (length sequence-of-vectors))
	 (sum-seq (make-array dimensions :element-type 'single-float :initial-element 0.0)))
    (loop for d from 0 below dimensions do
	 (loop for v from 0 below vector-count do
	      (let ((value (elt (elt sequence-of-vectors v) d)))
		(declare (single-float value))
		(incf (aref sum-seq d) value))))    
    (coerce sum-seq result-type)))

(defun vector-sequence-mean (result-type sequence-of-vectors)
  "Given a sequence of 'vectors', compute the mean and return it in the
   specified sequence type."
  (let* ((vector-count (length sequence-of-vectors))
	 (summed-vector (vector-sequence-add result-type sequence-of-vectors))
	 (average-vector (map result-type (lambda (x) (/ x vector-count)) summed-vector))) ;; this can be a map-into !!
    average-vector))

(defun vector-sequence-average (result-type sequence-of-vectors)
  "Synonym for VECTOR-SEQUENCE-MEAN."
  (vector-sequence-mean result-type sequence-of-vectors))

(defun compute-vector-covariance-diagonal (sequence-of-vectors &optional mean)
  "Compute the the diagonal of the co-variance matric of a sequence of vectors.
   Optionally takes the mean as an argument.  If this argument is excluded, the
   mean is computed by this function."
  (unless mean
    (setf mean (vector-sequence-mean 'vector sequence-of-vectors)))
  (let* ((vector-count (length sequence-of-vectors))
	 (dimension-count (length (elt sequence-of-vectors 0)))
	 (covar-vector (make-array dimension-count :element-type 'single-float)))
    (dotimes (dimension dimension-count) ;;for each dimension
      (let ((sum-of-squared-differences 0.0)
	    (dimension-mean (elt mean dimension)))
	(declare (single-float dimension-mean sum-of-squared-differences))
	(dotimes (vector-num vector-count)                       ;; for each vector in our sequence of vectors
	  (let* ((this-vector (elt sequence-of-vectors vector-num))
		 (this-vector-value (elt this-vector dimension))
		 (squared-diff (expt (- this-vector-value dimension-mean) 2)))
	    (declare (single-float this-vector-value))
	    (incf sum-of-squared-differences squared-diff)))
	(setf (elt covar-vector dimension) (/ sum-of-squared-differences vector-count))))
    covar-vector))
	    
(defun compute-vector-list-parameters (sequence-of-vectors)
  "Given a sequences of vectors compute the mean and covariance."
  (let* ((mean (vector-sequence-mean '(vector single-float) sequence-of-vectors))
	 (covariance (compute-vector-covariance-diagonal sequence-of-vectors mean)))
    (make-instance 'gaussian
		   :mean mean 
		   :variance covariance
		   :dimensions (length mean))))

(defun get-euclidean-distance (list1 list2)
  "Get the Euclidean distance between two lists of numbers."
  (let ((dimension-differences-squared-sum 0))
    (loop for e1 across list1
          for e2 across list2 do
	 (incf dimension-differences-squared-sum
	       (expt (- e1 e2) 2)))     ;;difference squared
    (sqrt dimension-differences-squared-sum)))
