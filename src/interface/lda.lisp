;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Reading Sphinx LDA transformation matrix files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read/write feature transformation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Read/write feature transformation")

(defun read-feature-transform-verbose (&key checksum-p checksum computed-checksum feature-stream-count
				       cepstral-feature-count transformed-feature-count data-length-given)
  "Print summary information when reading a feature transformation file."
  (write-line "**************************************************")
  (write-line "Feature transform summary:")
  (when checksum-p
    (format t "Checksum:~% ~:D~%" checksum)
    (format t "Computed checksum:~% ~:D~%" computed-checksum))
  (format t "Feature stream count:                       ~:D~%" feature-stream-count)
  (format t "Ceptral feature count:                      ~:D~%" cepstral-feature-count)
  (format t "Transformed feature count:                  ~:D~%" transformed-feature-count)
  (format t "Data length:                                ~:D~%" data-length-given))

(defun read-feature-transform (filename &key verbose)
  "Read in a Sphinx LDA feature transform file."
  (multiple-value-bind (header-length checksum-p)
      (get-header-and-offset filename)
    ;; Now read the content...
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 32))
      (let* ((file-length (file-length f))
	     (offset (+ (/ header-length 4) 1))
	     (seq (make-array (- file-length offset))))
	(file-position f offset)
	(read-sequence seq f)
 	(let* ((feature-stream-count (elt seq 0))      ;; is that what this is?
	       (cepstral-feature-count (elt seq 1))
	       (transformed-feature-count (elt seq 2)) ;; the number of features transformed into?
	       (data-length-given (elt seq 3))
	       (checksum (when checksum-p (alexandria:last-elt seq)))
	       (computed-checksum (when checksum-p 0))
	       (data (subseq seq 4 (+ data-length-given 4)))
	       ;;(data-array (make-array (list feature-stream-count cepstral-feature-count transformed-feature-count)))
	       (data-array (make-array (list cepstral-feature-count transformed-feature-count)))
	       (data-length (length data))
	       (computed-data-length (* feature-stream-count cepstral-feature-count transformed-feature-count)))
	  (assert (= data-length-given computed-data-length data-length))
	  (when verbose
	    (read-feature-transform-verbose :checksum-p checksum-p :checksum checksum :computed-checksum computed-checksum :feature-stream-count feature-stream-count 
					    :cepstral-feature-count cepstral-feature-count :transformed-feature-count transformed-feature-count :data-length-given data-length-given))
	  (setf data (map 'list 'ieee-floats:decode-float32 data))
	  (loop for j from 0 below cepstral-feature-count do
	       (loop for k from 0 below transformed-feature-count do
		    (setf (aref data-array j k) (pop data))))
	  data-array)))))

(defun print-feature-transform (data &optional (stream *standard-output*))
  "Print feature transformation data in a format similar to SphinxTrain's printp program."
  (declare ((simple-array float (* * *)) data))
  (declare (stream stream))
  (format stream "lda ~d ~d ~d~%" (array-dimension data 0) (array-dimension data 1) (array-dimension data 2))
  (loop for i from 0 below (array-dimension data 0) do
       (format stream "lda ~d~%" i)
       (loop for j from 0 below (array-dimension data 1) do
	    (loop for k from 0 below (array-dimension data 2) do
		 (format stream "~,6,2e " (aref data i j k)))
	    (terpri stream))))


