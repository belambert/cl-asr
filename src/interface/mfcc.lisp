;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Reading Sphinx MFCC extracted feature files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read an MFCC feature file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Read/write MFCC feature transformation files")


;; Not sure if this is right... try to find out?
(defun unextract-mfcc-features (mfccs &key (feature-count 13))
  "An attempt at writing a function which reverses the application of the linear transformation, and then
   lops off the velocity and acceleration features."
  (when (and *acoustic-model* (acoustic-model-transformation-matrix *acoustic-model*))      
    (let ((transformation-matrix (matrix-transpose (acoustic-model-transformation-matrix *acoustic-model*))))
      (setf mfccs (map 'vector (lambda (x) (matrix-multiply-1d x transformation-matrix)) mfccs))))
  (assert (every (lambda (x) (= (length x) (* feature-count 3))) mfccs))
  (setf mfccs (map 'vector (lambda (x) (subseq x 0 feature-count)) mfccs))
  mfccs)

;; Re-write this as a generic function...?
(defun split-data-into-frames (samples feature-count)
  "Given a long sequence of numbers (here referred to misleadingly as 'samples'),
   split that long sequence into a number of shorter sequences, each with feature-count
   number of numbers.  These comprise the 'features' of a particular 'frame.'"
  (assert (= (mod (length samples) feature-count) 0))
  (let ((frames '())
	(frame-count (/ (length samples) feature-count)))
    (dotimes (i frame-count)
      (push (subseq samples (* i feature-count) (* (1+ i) feature-count)) frames))
    (nreverse frames)))

;;(defun read-mfcc-file-internal1 (filename &key (feature-count 13))
(defun read-raw-mfccs (filename &key (feature-count 13))
  (with-open-file (f filename :direction :input :element-type '(unsigned-byte 32))
    (let* ((header (byte-swap (read-byte f)))
	   (file-length (file-length f))
	   (data-length (- file-length 1))
	   (data (make-array data-length)))
      (assert (= header data-length))
      (read-sequence data f)
      (map-into data 'byte-swap data)
      (map-into data 'ieee-floats:decode-float32 data)
      (split-data-into-frames data feature-count))))

(defun pad-mfccs (frames)
  (let* ((count (length frames))
	 (first-frame (elt frames 0))
	 (last-frame (elt frames (1- count))))
    (append (list first-frame first-frame first-frame)
	    frames
	    (list last-frame last-frame last-frame))))

(defun remove-padding (mfccs)
  (subseq mfccs 3 (- (length mfccs) 3)))

(defun vector-almost-equal (seq1 seq2 &key (tol 0.01))
  (loop for e1 across seq1
     for e2 across seq2
     for diff = (abs (- e1 e2)) do
       (unless (< diff tol)
	 (return-from vector-almost-equal nil)))
  t)

(defun read-mfcc-file (filename &key (feature-count 13) (ldadim nil))
  "Interface level function to read an MFCC file."
  (let* ((raw-mfccs (read-raw-mfccs filename :feature-count feature-count))
	 (normalized (cepstra-mean-normalization raw-mfccs))
	 (padded (pad-mfccs normalized))
	 (dynamic (dynamic-feature-computation padded))
	 (mfccs (remove-padding dynamic))
	 (lda (acoustic-model-transformation-matrix *acoustic-model*)))
    ;; Make sure everything is vectors...
    (setf mfccs (map 'vector (lambda (x) (coerce x '(simple-array single-float))) mfccs))
    (when lda
      (format t "Doing LDA transformation with ~dx~d transformation matrix~%" (array-dimension lda 0) (array-dimension lda 1))
      (setf mfccs (map 'vector (lambda (x) (matrix-multiply-1d x lda)) mfccs))
      (when ldadim
	(setf mfccs (map 'vector (lambda (x) (subseq x 0 ldadim)) mfccs))))
    mfccs))


;; This is a pretty generic function
(defun sequence-subtract (seq1 seq2 &key (start 0) (end nil) (length nil))
  "Subtract elements from seq2 from seq1, but only within the specified bounds."
  (when (and length (not end))
    (setf end (+ start length)))
  (setf seq1 (subseq seq1 start end))
  (setf seq2 (subseq seq2 start end))
  (map 'vector '- seq1 seq2))

(defun dynamic-feature-computation (data)
  "Convert a sequence of cepstral features, to a sequence of cepstral features with velocity,
   and acceleration features."
  (let* ((frame-count (length data))
	 (cep-count (length (elt data 0)))
	 (frames (make-array frame-count :element-type 'simple-array :initial-element #()))
	 (feature-count (* cep-count 3)))
    ;; Create some empty vectors, and copy the base cepstral features
    (loop for i from 0 below frame-count
       for feature-array = (make-array feature-count :element-type 'single-float :initial-element 0.0) do
	 (setf (aref frames i) feature-array)
	 (replace feature-array (elt data i)))

    ;; Compute velocity features
    (loop for i from 2 below (- frame-count 2)
	 for frame = (aref frames i) do
	 (replace frame (sequence-subtract (aref frames (+ i 2)) (aref frames (- i 2)) :length cep-count)
		  :start1 cep-count))
    ;; First 2 columns velocity
    (replace (aref frames 0) (aref frames 2) :start1 cep-count :start2 cep-count) ;; :end1 (* cep-count 2) :end2 (* cep-count 2))
    (replace (aref frames 1) (aref frames 2) :start1 cep-count :start2 cep-count) ;; :end1 (* cep-count 2) :end2 (* cep-count 2))    
    ;; Last 2 columns velocity
    (replace (aref frames (- frame-count 1)) (aref frames (- frame-count 3)) :start1 cep-count :start2 cep-count) ;; :end1 (* cep-count 2) :end2 (* cep-count 2))
    (replace (aref frames (- frame-count 2)) (aref frames (- frame-count 3)) :start1 cep-count :start2 cep-count) ;; :end1 (* cep-count 2) :end2 (* cep-count 2))    
   
    ;; Compute acceleration features
    (loop for i from 1 below (- frame-count 1)
	 for frame = (aref frames i) do
	 (replace frame (sequence-subtract (aref frames (+ i 1)) (aref frames (- i 1)) :start cep-count :length cep-count)
		  :start1 (* cep-count 2)))
    ;; First and last column acc
    (replace (aref frames 0) (aref frames 1) :start1 (* cep-count 2) :start2 (* cep-count 2)) ;; :end1 (* cep-count 2) :end2 (* cep-count 2))
    (replace (aref frames (- frame-count 1)) (aref frames (- frame-count 2)) :start1 (* cep-count 2) :start2 (* cep-count 2)) ;; :end1 (* cep-count 2) :end2 (* cep-count 2))

    frames))

(defun print-mfccs (frames)
  "Print an MFCC file readably."
  (dolist (frame frames)
    (format t "~{ ~8,3F~}~%" (coerce frame 'list))))


