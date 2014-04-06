;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Visualization of audio and trellises")

(defun cepstra->spectra (cepstra &key (log t))
  "Convert cepstrum features into spectrum features using an Inverse Discerete Cosine Transform."
  (let ((spectra '()))
    (loop for cepstrum in cepstra
       for vector =  (make-array 128 :element-type 'single-float :initial-element 0.0) do	
	 (map-into vector 'identity cepstrum)
	 (let* ((idct (idct vector)))
	   (setf idct (coerce idct 'list))
	   (unless log
	     (setf idct (mapcar (lambda (x) (expt 1.5 x)) idct)))
	   (push (coerce idct 'list) spectra)))
    (nreverse spectra)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  GNUPLOT SETUP/USE FUNCTIONS   ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Low-level function that interface with gnuplot")

(defun spectral-analyses->lists (spectral-analyses)
  "Convert a list of spectral analysis structs into a list of lists that we can give to the heat mapper."
  (let ((lists '())
	(tmp-list '()))
    (dotimes (y (length (spectral-analysis-magnitude-list (first spectral-analyses))))
      (dotimes (x (length spectral-analyses))
	(push (elt (spectral-analysis-magnitude-list (elt spectral-analyses x)) y)
	      tmp-list))
      (push (reverse tmp-list) lists)
      (setf tmp-list '()))
    (reverse lists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Visualization of spectra/cepstra ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Visualization of spectra/cepstra")

(defun show-spectrogram (audio &optional filename)
  "Show the spectrogram for some audio."
  (let* ((audio-frames (split-audio-into-frames audio))
	 (fft-analyses (mapcar 'zero-pad-and-fft-audio-segment audio-frames))
	 (spectral-analyses (mapcar 'dft-analysis->frequency-band-analysis fft-analyses))
	 (data-points (spectral-analyses->lists spectral-analyses)))
    (gnuplot:plot-graph data-points :heatmap :title "Spectrogram" :filename filename)))

(defun show-mel-feature-values (audio &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) filename)
  "Show the Mel features values for some audio."
  (let* ((audio-frames (split-audio-into-frames audio))
	 (fft-analyses (mapcar 'zero-pad-and-fft-audio-segment audio-frames))
	 (mel-feature-values (mapcar (lambda (x) (get-log-mel-feature-values x filter-bank-size :start-freq start-freq :end-freq end-freq)) fft-analyses)))
    (setf mel-feature-values (invert-list-array mel-feature-values))
    (gnuplot:plot-graph mel-feature-values :heatmap :title "Mel feature values" :filename filename)))

(defun show-mel-cepstrum (audio &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) filename (cepstrum-count 13))
  "Show the mel cepstral features for some audio."
  (let* ((mel-cepstrum-features (get-mel-cepstrum audio :filter-bank-size filter-bank-size :start-freq start-freq :end-freq end-freq :cepstrum-count cepstrum-count))
	 (inverted-mel-cepstrum-features (mapcar (lambda (x) (mel-cepstrum-features->mel-features x filter-bank-size)) mel-cepstrum-features)))
    (setf inverted-mel-cepstrum-features (invert-list-array inverted-mel-cepstrum-features))
    (gnuplot:plot-graph inverted-mel-cepstrum-features :heatmap :title "Mel cepstrum" :filename filename)))

(defun show-complete-feature-set (audio &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) filename (cepstrum-count 13))
  "Show the mel cepstral features for some audio."
  (let* ((feature-array (get-mfcc-features audio :filter-bank-size filter-bank-size :start-freq start-freq :end-freq end-freq :cepstrum-count cepstrum-count)))
    (setf feature-array (invert-list-array feature-array))
    (gnuplot:plot-graph feature-array :heatmap :title "Complete feature set" :filename filename)))

;; (defun show-mel-cepstrum-with-hypothesis (audio shortest-path &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13))
;;   "Show the mel cepstral features for some audio."
;;   (declare (ignore start-freq end-freq cepstrum-count))
;;   (let* ((mel-cepstrum-features (cond ((typep audio 'mfcc-sequence)
;; 				       (map 'list (lambda (x) (subseq x 0 13)) (mfcc-sequence-mfcc-seq audio)))
;; 				      ((vectorp audio)
;; 				       audio)
;; 				      (t (error "Invalid audio format..."))))
;; 	 (inverted-mel-cepstrum-features (invert-list-array (map 'list (lambda (x) (mel-cepstrum-features->mel-features x filter-bank-size)) mel-cepstrum-features)))
;; 	 (word-list shortest-path)
;; 	 (word-coordinates (get-word-frame-coordinates word-list)))
;;     (setf word-list (substitute 0 NIL word-list))
;;     (setf word-list (substitute 1 "silence" word-list :test 'equalp))
;;     (setf word-list (substitute 1 "<sil>" word-list :test 'equalp))
;;     (setf word-list (substitute-if 2 'stringp word-list))
;;     (show-hypothesis-and-heat-map inverted-mel-cepstrum-features word-list word-coordinates :title "Mel cepstrum with hypothesis")
;;     (values)))

(defun show-hypothesis-and-heat-map (spectra back-pointers &key (title "Title") save-image-file (type :aqua))
  "Print a spectrogram data, including the hypothesis."
  (with-output-to-string (s)
    ;; Draw vertical lines and create the word labels
    (loop for bp in back-pointers
       for i from 0 below (length back-pointers)
       for word = (back-pointer-word-id bp) do
    	 (when (and word (not (string-equal word "silence")))
    	   ;;(setf word (cleanup-phone-string word))
    	   (format s "set label ~A \"~A\" at ~A,10 front rotate left nopoint tc rgb \"#000000\"~%" (1+ i) word (1+ (back-pointer-begin-time bp)))
    	   (format s "set arrow from ~d,-10 to ~d,138 nohead front~%" (back-pointer-begin-time bp) (back-pointer-begin-time bp))))
    (let ((frames (length (first spectra)))
	  (freq-bands (length spectra)))
      (gnuplot:plot-graph spectra :heatmap :manual-options (get-output-stream-string s) :debug t :filename save-image-file :width (* 5 frames) :height (* 5 freq-bands) :title title :type type))))
  
(defun show-spectrum-with-hypothesis (mfccs bps &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13) (filename "./spectrogram-with-hyp") title (type :aqua))
  "Show the mel cepstral features for some audio."
  (declare (ignore start-freq end-freq cepstrum-count filter-bank-size))
  (let* ((spectra (invert-list-array (cepstra->spectra (coerce mfccs 'list)))))
    (show-hypothesis-and-heat-map spectra bps :title title :save-image-file filename :type type)))

(defun get-word-frame-coordinates (word-list)
  "Get the frame start locations of this 'word list'.  This isn't really a word list,
   rather it's a list of which word is predicted for each frame."
  (let ((frame-counter 0)
	(previous-word nil)
	(coordinate-list '()))
    (dolist (word word-list)
      (when (not (equal word previous-word))
	(push (list word frame-counter)
	      coordinate-list)
	(setf previous-word word))
      (incf frame-counter))
    coordinate-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  TRELLIS VIZUALIZATION   ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Visualization of the trellis.")

(defun print-trellis-new (trellis-array &key filename (normalize-score nil))
  "Print a graphical representation of a trellis."
  (declare (ignore normalize-score))
  (let ((list-array (invert-list-array (2d-array->list-array trellis-array))))
    (gnuplot:plot-graph list-array :heatmap :title "Trellis" :filename filename)))

(defun 2d-array->list-array (array)
  "Convert a 2d array into an embedded lists style array."
  (let ((row-list '()))
    (loop for x from (1- (array-dimension array 0)) downto 0 do
      (let ((row '()))
	(loop for y from (1- (array-dimension array 1)) downto 0 do
	     (let ((this-value (aref array x y)))
	       (push (max this-value 0.0) row)))
	(push row row-list)))
    row-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Visualize language HMM ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Visualize language HMM")

(cl-user::todo "Better to visualize it with OpenFST?")

(defun language-hmm->sif-file (hmm sif-filename)
  "Convert a language HMM to sif file format.  SIF network files can be visualized with the program 'Cytoscape'."
  (with-open-file (file sif-filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for i from 0 below (length (language-hmm-transitions hmm))
       for entry across (language-hmm-transitions hmm) do
	 (dolist (j entry)
	   (let ((source (if (elt (language-hmm-hmm-state-words hmm) i)
			     (format nil "~{~A~^-~}-~A" (elt (language-hmm-word-state-map hmm) i) i)
			     i))
		 (dest (if (elt (language-hmm-hmm-state-words hmm) j)
			   (format nil "~{~A~^-~}-~A" (elt (language-hmm-word-state-map hmm) j) j)
			   j)))
	   (format file "~A ~A ~A~%" source "edge" dest))))))


