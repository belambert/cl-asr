;;;; Author: Ben Lambert
;;;; ben@benjaminlambert

(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read/write Sphinx Gaussian mixture weights ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-mixture-weights-verbose (&key checksum-p checksum computed-checksum state-count feature-stream-count density-count data-length-given)
  "Print summary information while reading a Sphinx mixture weight file."
  (write-line "**************************************************")
  (write-line "Mixture weight summary:")
  (when checksum-p
    (format t "Checksum:~% ~:D~%" checksum)
    (format t "Computed checksum:~% ~:D~%" computed-checksum))
  (format t "State count:                                ~:D~%" state-count)
  (format t "Feature stream count                        ~:D~%" feature-stream-count)
  (format t "Density count (gaussians per mixture):      ~:D~%" density-count)
  (format t "Data length:                                ~:D~%" data-length-given))

(defun read-mixture-weights (filename &key verbose)
  "Read a Gaussian mixture weights file."
  (multiple-value-bind (header-length checksum-p)
      (get-header-and-offset filename)
    ;; Now read the content...
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 32))
      (let* ((file-length (file-length f))
	     (offset (+ (/ header-length 4) 1))
	     (seq (make-array (- file-length offset))))
	(file-position f offset)
	(read-sequence seq f)
 	(let* ((state-count (elt seq 0))             ;; aka mgau -- the number of states in the model
	       (feature-stream-count (elt seq 1))    ;; This is usually just a "1"? 
	       (density-count (elt seq 2))           ;; This is the number of Gaussians in the mixture
	       (data-length-given (elt seq 3))       ;; Finally, the number of numbers (the size of the data payload) is given
	       (checksum (when checksum-p (alexandria:last-elt seq)))
	       (computed-checksum (when checksum-p 0))
	       (data (subseq seq 4 (+ data-length-given 4)))
	       (weight-arrays '())
	       (data-length (length data))
	       (computed-data-length (* state-count feature-stream-count density-count)))
	  (declare (type fixnum state-count feature-stream-count density-count data-length-given))
	  (when verbose
	    (read-mixture-weights-verbose :checksum-p checksum-p :checksum checksum :computed-checksum computed-checksum :state-count state-count :feature-stream-count feature-stream-count :data-length-given data-length-given))
	  (assert (= data-length-given computed-data-length data-length))
	  (setf data (map 'list 'ieee-floats:decode-float32 data))
	  (loop for i from 0 below state-count do
	       (loop for j from 0 below feature-stream-count do
		  ;; WE'RE NORMALIZING AS WE READ THESE IN.... (the non-normalized values are supposedly useful at other places *during* training)
		    (let ((weight-sum 0.0)
			  (weight-array (make-array density-count :element-type 'single-float)))
		      ;; Save all the weights, summing them.
		      (loop for k from 0 below density-count do
			   (setf (aref weight-array k) (pop data))
			   (incf weight-sum (aref weight-array k)))
		      ;; Divide by the sum, so that they sum to one.
		      (loop for k from 0 below density-count do
			   (setf (aref weight-array k) (/ (aref weight-array k) weight-sum)))
		      (push weight-array weight-arrays))))
	  (nreverse weight-arrays))))))

(defun print-mixture-weights-to-file (data filename)
  "Print Gaussian mixture weights in a format similar to SphinxTrain's printp program."
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (print-mixture-weights data f)))

(defun print-mixture-weights (data &optional (stream *standard-output*))
  "Print Gaussian mixture weights in a format similar to SphinxTrain's printp program."
  (declare (optimize (speed 3)))
  (declare ((simple-array float (* * *)) data))
  (declare (stream stream))
  (format stream "mixw ~d ~d ~d~%" (array-dimension data 0) (array-dimension data 1) (array-dimension data 2))
  (loop for i from 0 below (array-dimension data 0) do
       (loop for j from 0 below (array-dimension data 1) do
	    (format stream "mixw [~d ~d] ~f~%" i j 0.0)
	    (loop for k from 0 below (array-dimension data 2) do
		 (when (= (mod k 8) 0)
		   (format stream"~%        "))
		 (format stream "~,3,2e " (aref data i j k)))
	    (terpri stream))))
