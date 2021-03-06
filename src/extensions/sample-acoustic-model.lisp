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

(in-package :cl-asr)

;;; These don't/won't use the velocity/acceleration features...
(defgeneric sample (distribution)
  (:documentation "Sample the given distribution."))

(defmethod sample ((gaussian gaussian))
  "This samples a basic (multi-dimensional) gaussian distibution (but not a mixture)."
  (let ((sample (make-array (gaussian-dimensions gaussian) :element-type 'float :initial-element 0.0)))
    (dotimes (i (gaussian-dimensions gaussian))
      (let ((random (+ (* (alexandria:gaussian-random) (aref (gaussian-variance gaussian) i))
		       (aref (gaussian-mean gaussian) i))))
	(setf (aref sample i) random)))
    (coerce sample 'list)))

(defmethod sample ((dist gaussian-mixture))
  "This samples a basic (multi-dimensional) gaussian distibution (but not a mixture)."
  (let ((gaussian-index (select-random-array-index (gaussian-mixture-weights dist))))
    (sample (aref (gaussian-mixture-gaussians dist) gaussian-index))))

;; TODO - These two can/should move to somewhere more generic...
(defun get-array-column (array column)
  "Get the specified column of a 2d array as a vector."
  (let ((values '()))
    (dotimes (i (array-dimension array 0))
      (push (aref array i column) values))
    (make-array (array-dimension array 0) :initial-contents (nreverse values))))

(defun get-array-row (array row)
  "Get the specified row of a 2d array as a vector."
  (let ((values '()))
    (dotimes (i (array-dimension array 1))
      (push (aref array row i) values))
    (make-array (array-dimension array 1) :initial-contents (nreverse values))))

(defun select-random-array-index (array)
  "Given an array where each value in the array represents a probability
   (and, presumably, the values sum to one), randomly select one of the 'buckets' (i.e. entries
   in the array) proportional to its probability."
  (let ((random (random 1.0))
	(sum 0.0))
    (dotimes (i (length array))
      (incf sum (elt array i))
      (when (> sum random)
	(return-from select-random-array-index i)))
    nil))

(defun sample-hmm (hmm)
  "Given an HMM, generate a random sample."
  (let ((observations '())
	(state-num 0))
    ;; Loop until we get to the last state of the HMM
    (loop until (= state-num (hmm-state-count hmm))
       for observation = (sample (elt (hmm-emission-distributions hmm) state-num))
       for transition-array = (get-array-row (hmm-transition-probabilities hmm) state-num) do
	 ;; Save the observation we sampled.
	 (push observation observations)
	 ;; Randomly transition to another state.
	 (setf state-num (select-random-array-index transition-array)))
    (nreverse observations)))
