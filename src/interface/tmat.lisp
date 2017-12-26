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

(defun read-transition-matrices-verbose (&key checksum-p checksum computed-checksum hmm-count state-count transition-count data-length-given)
  "Print summary information while reading a Sphinx transition matrix file."
  (write-line "**************************************************")
  (write-line "Transition matrices summary:")
  (when checksum-p
    (format t "Checksum:~% ~:D~%" checksum)
    (format t "Computed checksum:~% ~:D~%" computed-checksum))
  (format t "HMM count:                                  ~:D~%" hmm-count)
  (format t "State count:                                ~:D~%" state-count)
  (format t "Transition count                            ~:D~%" transition-count)
  (format t "Data length:                                ~:D~%" data-length-given))

(defun read-transition-matrices (filename &key verbose)
  "There aren't many parameters here...
   It's just about 3 numbers per *phoneme*
   so about 50 times 3 numbers = about 150."
  (multiple-value-bind (header-length checksum-p)
      (get-header-and-offset filename)
    ;; Now read the content...
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 32))
      (let* ((file-length (file-length f))
	     (offset (+ (/ header-length 4) 1))
	     (seq (make-array (- file-length offset))))
	(file-position f offset)
	(read-sequence seq f)
 	(let* ((hmm-count (elt seq 0))             ;; aka mgau -- the number of states in the model
	       (state-count (elt seq 1))    
	       (transition-count (elt seq 2))
	       (data-length-given (elt seq 3))     ;; Finally, the number of numbers (the size of the data payload) is given
	       (checksum (when checksum-p (alexandria:last-elt seq)))
	       (computed-checksum (when checksum-p 0))
	       (data (subseq seq 4 (+ data-length-given 4)))
	       (tmats '())
	       (data-length (length data))
	       (computed-data-length (* hmm-count state-count transition-count)))
	  (declare (type fixnum hmm-count state-count transition-count data-length-given))
	  (when verbose
	    (read-transition-matrices-verbose :checksum-p checksum-p :checksum checksum :computed-checksum computed-checksum :hmm-count hmm-count
					      :state-count state-count :transition-count transition-count :data-length-given data-length-given))
	  (assert (= data-length-given computed-data-length data-length))
	  (setf data (map 'list 'ieee-floats:decode-float32 data))
	  (loop for i from 0 below hmm-count do
	       (let ((tmat (make-array (list state-count transition-count))))
		 (loop for j from 0 below state-count do
		      (let ((weight-sum 0.0))
			(loop for k from 0 below transition-count do
			     (setf (aref tmat j k) (pop data))
			     (incf weight-sum (aref tmat j k)))
			;; Divide by the sum, so that they sum to one.
			(loop for k from 0 below transition-count do
			     (setf (aref tmat j k) (/ (aref tmat j k) weight-sum)))))
		 ;; Take the transpose...?!?!
		 ;;(setf tmat (matrix-transpose tmat))
		 (push tmat tmats)))
	  (nreverse tmats))))))

(defun print-transition-matrices-to-file (data filename)
  "Print transition matrix probability data in a format similar to SphinxTrain's printp program."
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (print-transition-matrices data f)))

(defun print-transition-matrices (data &optional (stream *standard-output*))
  "Print transition matrix probability data in a format similar to SphinxTrain's printp program."
  (declare ((simple-array float (* * *)) data))
  (declare (stream stream))
  (format stream "tmat ~d ~d~%" (array-dimension data 0) (array-dimension data 2))
  (loop for i from 0 below (array-dimension data 0) do
       (format stream "tmat [~d]~%" i)
       (loop for j from 0 below (array-dimension data 1) do
	    (loop for k from 0 below (array-dimension data 2) do
		 ;; This printing is a little funny, and doesn't quite line up.
		 ;; This is intentional so that it matches the Sphinx printp printout, for comparison.
		 (if (/= (aref data i j k) 0)
		     (format stream " ~9,3,2e" (aref data i j k))
		     (format stream "~9A" "")))
	    (terpri stream))))
