;;;; Author: Ben Lambert (ben@benjaminlambert)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Reading Sphinx gaussian model files (means or variances).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read/write Sphinx Gaussian files - either means or variances ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Read/write Sphinx Gaussian files - either means or variances")

(defun read-gaussian-verbose (&key checksum-p checksum computed-checksum state-count feature-stream-count density-count cepstral-feature-count data-length-given)
  "Print summary information while reading a Sphinx Gaussian file."
  (write-line "**************************************************")
  (write-line "Gaussian summary:")
  (when checksum-p
    (format t "Checksum:~% ~:D~%" checksum)
    (format t "Computed checksum:~% ~:D~%" computed-checksum))
  (format t "State count:                                ~:D~%" state-count)
  (format t "Feature stream count                        ~:D~%" feature-stream-count)
  (format t "Density count (gaussians per mixture):      ~:D~%" density-count)
  (format t "Cepstral feature count:                     ~:D~%" cepstral-feature-count)
  (format t "Data length:                                ~:D~%" data-length-given))

(defun read-gaussian (filename &key verbose)
  "Read a binary Sphinx 'gaussian' file, representing either means or variances."
  ;; Get the header length and read the header.
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
	       (feature-stream-count (elt seq 1))           ;; This is usually just a "1"? 
	       (density-count (elt seq 2))           ;; This is the number of Gaussians in the mixture
	       (cepstral-feature-count (elt seq 3))  ;; This is the number of cepstral features --  this would be several values if feature-count != 1
	       (data-length-given (elt seq 4))       ;; Finally, the number of numbers (the size of the data payload) is given
	       (checksum (when checksum-p (alexandria:last-elt seq)))
	       ;;(computed-checksum (when checksum-p (compute-checksum seq)))
	       (computed-checksum (when checksum-p 0))
	       (data (subseq seq 5 (+ data-length-given 5)))
	       (vector-list '())
	       (data-length (length data))
	       (computed-data-length (* state-count feature-stream-count density-count cepstral-feature-count))
	       (data-index 0))
	  (declare (type fixnum state-count feature-stream-count density-count cepstral-feature-count data-length-given))
	  (when verbose
	    (read-gaussian-verbose :checksum-p checksum-p :checksum checksum :computed-checksum computed-checksum :state-count state-count :feature-stream-count feature-stream-count
				   :density-count density-count :cepstral-feature-count cepstral-feature-count :data-length-given data-length-given))
	  (assert (= data-length-given computed-data-length data-length))
	  (setf data (map-into data 'ieee-floats:decode-float32 data))
	  (loop for i from 0 below state-count do
	       (let ((state-means '()))
		 (loop for j from 0 below feature-stream-count do
		    ;; for each gaussian in the mixture...
		      (loop for k from 0 below density-count do
			   (let ((vector (make-array cepstral-feature-count :element-type 'single-float))) ;; This is the actual mean/variance array
			     (loop for l from 0 below cepstral-feature-count do
				  (let ((element (aref data data-index)))
				    (incf data-index)
				    (setf (aref vector l) element)))
			     (push vector state-means))))
		 (push (nreverse state-means) vector-list)))
	  (nreverse vector-list))))))

(defun print-gaussian-to-file (data filename)
  "Print Gaussian parameters (either means or variances), in a format similar to SphinxTrain's printp program."
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (print-gaussian data f)))

(defun print-gaussian (data &optional (stream *standard-output*))
  "Print Gaussian parameters (either means or variances), in a format similar to SphinxTrain's printp program."
  (declare (optimize (speed 3)))
  (declare ((simple-array float (* * * *)) data))
  (declare (stream stream))
  (format stream "param ~d ~d ~d~%" (array-dimension data 0) (array-dimension data 1) (array-dimension data 2))
  (loop for i from 0 below (array-dimension data 0) do
       (format stream "mgau ~d~%" i)
       (loop for j from 0 below (array-dimension data 1) do
	    (format stream "feat ~d~%" j)
	    (loop for k from 0 below (array-dimension data 2) do
		 (format stream "density~5d " k)
		 (loop for l from 0 below (array-dimension data 3) do
		      (format stream "~,3,2e " (aref data i j k l)))
		 (format stream "~%")))))

