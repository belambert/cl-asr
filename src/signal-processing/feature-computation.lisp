;;;; Author: Ben Lambert (ben@benjaminlambert)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Computing MFCC features")

(defstruct mfcc-sequence
  "Defines the computed features for an audio segment, along with as much meta-data as possible."
  seconds
  id
  source
  sample-rate
  sample-resolution
  transcript
  mfcc-seq
  filter-bank-size
  start-freq
  end-freq
  cepstrum-count
  preemphasis
  cepstra-mean-normalization
  frame-length
  frame-delta
  filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; RECORDING AND PLAYBACK  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Recording and playback")

(defun print-dft-analysis (struct stream depth)
  "Simple helper function for printing a DFT analysis object.
   This prints some simple statistics (i.e. counts) rather than huge numbers of numbers."
  (declare (ignore depth))
  (format stream "[dft-analysis: ~A coefficients, orig-sampling-rate: ~A]"
	  (length (dft-analysis-coefficient-list struct))
	  (dft-analysis-original-sampling-rate struct)))


(defstruct (dft-analysis (:print-function print-dft-analysis))
  "A list of DFT coefficients, plus the original sampling rate which is necessary in order to use the DFT results.
   DFT = discrete fourier transform."
  (coefficient-list nil :type (simple-array single-float))
  (original-sampling-rate 0 :type integer))

(defun print-spectral-analysis (struct stream depth)
  "Simple helper function for printing a spectral analysis object.
   This prints some simple statistics (i.e. counts) rather than huge numbers of numbers."
  (declare (ignore depth))
  (format stream "[spectral-analysis: ~A bands]"
	  (length (spectral-analysis-magnitude-list struct))))

(defstruct (spectral-analysis (:print-function print-spectral-analysis))
  "A simpler representation of the FFT results.  Here, we just have a list of values, and a list of the corresponding coefficients to which those values belong."
  magnitude-list
  frequency-range-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; SAMPLE PROCESSING FUNCTIONS... ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Sample processing functions")

(defun split-audio-into-frames (audio &key (frame-length 25) (frame-delta 10) verbose)
  "Given an audio segment, chop it up into frames."
  (let* ((total-sample-count (length (audio-segment-samples audio)))
	 (sample-rate (audio-segment-sample-rate audio))
	 (samples-in-frame (floor (* (/ frame-length 1000) sample-rate)))
	 (samples-in-frame-delta (floor (* (/ frame-delta 1000) sample-rate)))
	 (segmented-samples (chop-samples-into-frames (audio-segment-samples audio) total-sample-count samples-in-frame samples-in-frame-delta))
	 (new-audio-segment-list '()))    
    (dolist (sample-segment segmented-samples)
      (push
       (make-audio-segment :seconds (/ frame-length 1000)
			   :sample-rate sample-rate
			   :source (format nil "Segmented into ~Ams segments with ~Ams deltas" frame-length frame-delta)
			   :sample-resolution (audio-segment-sample-resolution audio)
			   :samples sample-segment)
       new-audio-segment-list))
    (when verbose (format t "Segmented audio into ~A segments, of length ~Ams offset by ~Ams.~%" (length new-audio-segment-list) frame-length frame-delta))
    (reverse new-audio-segment-list)))

(defun chop-samples-into-frames (samples total-sample-count samples-in-frame samples-in-frame-delta)
  "Given a list of samples, chop those into frames."
  (let ((frame-count (floor (/ total-sample-count samples-in-frame-delta)))
	(frame-sample-list '()))
    (dotimes (frame (- frame-count 2))
      (let* ((begin-sample-num (* frame samples-in-frame-delta))
	     (end-sample-num (+ begin-sample-num samples-in-frame))
	     (frame-samples (subseq samples begin-sample-num end-sample-num)))	
	(push frame-samples frame-sample-list)))
    (reverse frame-sample-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PREEMPHASIS  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Pre-emphasis")

(defun preemphasize-sample-list (sample-list &optional (alpha 0.95))
  "Given a list of samples perform 'pre-emphasis' which effectively boosts the volume of
   the higher frequencies in the signal.  Equation is something like:
   emphasized_sample[i] = sample[i] - alpha * sample[i-1]"
  (let ((pre-emphasized-samples (copy-seq sample-list)))
    (loop for i from 1 to (1- (length sample-list)) do	 
	 (setf (elt pre-emphasized-samples i)
	       (- (elt sample-list i)
		  (* alpha (elt sample-list (1- i))))))
    pre-emphasized-samples))

(defun preemphasize-audio (audio &optional (alpha 0.95))
  "Given an audio segment, chop it up into frames."
  (setf (audio-segment-samples audio) 
	(preemphasize-sample-list (audio-segment-samples audio) alpha))
  audio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; CEPSTRA MEAN NORMALIZATION  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Cepstra mean normalization")

(defun cepstra-mean-normalization (cepstra-list)
  "Given a list of ceptra vectors, compute the mean of all the vectors, then subtract the
   mean from each of the vectors."
  (let ((cepstra-mean (vector-sequence-mean 'vector cepstra-list)))
    (format t "CEPSTRAL MEAN:~{ ~F~}~%" (coerce cepstra-mean 'list))
    (map-into cepstra-list (lambda (x) (map-into x '- x cepstra-mean)) cepstra-list)
    cepstra-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; MEL FILTER/MFCC CODE ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Mel filter/ MFCC code")

(defun mel (f)
  "The mel function."
  (* 2595 (log (1+ (/ f 700) ) 10)))

(defun inverse-mel (f)
  "The inverse mel function."
  (* 700 (- (expt 10 (/ f 2595)) 1 )))

(defstruct mel-filter
  "Represents the beginning, end, and peak frequencies of a mel filter."
  (begin 0.0 :type single-float)
  (middle 0.0 :type single-float)
  (end 0.0 :type single-float))

(defun get-mel-filter-bank-coordinates (bank-count &key (start-freq 100) (end-freq 4000)) ; (filter-width 100))
  "A function to generate the necessary filters."
  (let* ((start-freq (mel start-freq))
	 (end-freq (mel end-freq))
	 (freq-delta (/ (- end-freq start-freq) bank-count))
	 (mel-filters '()))
    (declare (type single-float start-freq end-freq))
    (dotimes (i bank-count)
      (let ((center-freq (+ (* i freq-delta) start-freq))) ;; find the center frequency
	(push (make-mel-filter :begin (inverse-mel (- center-freq freq-delta))  ;; we call the inverse-mel function right away.... 
			       :middle (inverse-mel center-freq)
			       :end (inverse-mel (+ center-freq freq-delta)))
	      mel-filters)))
    (nreverse mel-filters)))

(blambert-util::memoize 'get-mel-filter-bank-coordinates)

(defun mel-filter (begin middle end frequency value)
  "Apply the mel filter triplet, at the given frequency to the given value."
  (declare (single-float begin middle end frequency value))
  (let ((step-size (/ 1 (- middle begin))))
    (the single-float
      (cond ((or (< frequency begin)
		 (> frequency end))
	     0.0)
	    ((<= frequency middle)
	     (* value (* step-size (- frequency begin))))
	    ((> frequency middle)
	     (* value (* step-size (- end frequency ))))
	    (t 0.0)))))

(defun inverse-mel-filter (begin middle end frequency value)
  "Apply the mel filter triplet, at the given frequency to the given value."
  (let ((step-size (/ 1 (- middle begin))))
    (cond ((or (< frequency begin)
	       (> frequency end))
	   0)
	  ((<= frequency middle)
	   (/ value (* step-size (- frequency begin))))
  	  ((> frequency middle)
	   (/ value (* step-size (- end frequency )))))))
    
(defun get-frequency-of-fft-coefficient (index dft-analysis)
  "Given an FFT analysis, and the index of one of the cooffcients, get the corresponding frequency."
  (declare (optimize (speed 3)))
  (let* ((coefficient-count (length (dft-analysis-coefficient-list dft-analysis)))
	 (sampling-rate (dft-analysis-original-sampling-rate dft-analysis))
	 (frequency (get-frequency-of-nth-fft-coefficient index coefficient-count sampling-rate)))
    (the single-float frequency)))

(defun get-frequency-of-nth-fft-coefficient (n coefficient-count sampling-rate)
  "Given an FFT analysis, and the index of one of the cooffcients, get the corresponding frequency.
   Note: THIS GETS MEMOIZED"
  (declare (type integer n coefficient-count sampling-rate))
  (setf coefficient-count (* coefficient-count 2))  ;; multiply by 2 because we cut the list in half earlier..?! ;;; this is a hack
  (let* ((ratio (/ (- n 1) coefficient-count))
	 (frequency (* ratio sampling-rate))
	 (float-frequency (float frequency)))
    (the single-float float-frequency)))

(blambert-util::memoize 'get-frequency-of-nth-fft-coefficient)
	

(defun convert-mel-filter-coordinates-to-mask-array (filter sample-count sampling-rate)
  "Convert a Mel filter into a FFT coefficient mask array.  This way we can simply apply the filter by
   taking the dot product of the mask and the output of the DFT.
   Note: THIS GETS MEMOIZED"
  (let* ((begin-freq (mel-filter-begin filter))
	 (peak-freq (mel-filter-middle filter))
	 (end-freq (mel-filter-end filter))
	 (mask (make-array sample-count :element-type 'single-float)))
    (dotimes (i sample-count)
      (unless (= i 0) ;;???
	(let ((frequency (get-frequency-of-nth-fft-coefficient i sample-count sampling-rate)))
	  (setf (aref mask i)
		(mel-filter begin-freq peak-freq end-freq frequency 1.0)))))
    mask))

(blambert-util::memoize 'convert-mel-filter-coordinates-to-mask-array)

(defun dot-product (a b)
  "An optimized dot product function.  Requires that the vectors be simple arrays of single-floats between
   the value of 0.0 and 1.0."
  (declare (optimize (speed 3))
	   (type (simple-array single-float) a b))
  (let ((sum 0.0))
    (loop for i from 0 to (1- (length a)) do
	 (let ((x (aref a i))
	       (y (aref b i)))
	   (incf sum (* x y))))
    (the single-float sum)))

(defun apply-mel-filter-to-fft-result (filter dft-analysis)
  "Apply a mel filter to a DFT analysis."
  (declare (optimize (speed 3)))
  (let* ((fft-result-list (dft-analysis-coefficient-list dft-analysis))
	 (sample-count (length (dft-analysis-coefficient-list dft-analysis)))
	 (sampling-rate (dft-analysis-original-sampling-rate dft-analysis))
	 (mask-filter (convert-mel-filter-coordinates-to-mask-array filter sample-count sampling-rate)))
    (dot-product mask-filter fft-result-list)))

(defun get-mel-feature-values (dft-analysis filter-bank-size &key (start-freq 100) (end-freq 4000))
  "Given an FFT analysis, apply the specified number of mel filters (evenly spaced in the mel frequency domain(?)) between the start
   and end frequency values.
   Note: This gets called on every frame:"
  (let* ((filter-bank (get-mel-filter-bank-coordinates filter-bank-size :start-freq start-freq :end-freq end-freq)) ;; this seems like a big one, but it's not...
	 (filter-values (make-array (length filter-bank) :element-type 'single-float)))
    (dotimes (i (length filter-bank))
      (let* ((filter (elt filter-bank i))
	     (filter-value (the single-float (apply-mel-filter-to-fft-result filter dft-analysis))))
	(declare (type single-float filter-value))
	(setf (elt filter-values i) filter-value)))
    filter-values))

(defun get-log-mel-feature-values (dft-analysis filter-bank-size &key (start-freq 100) (end-freq 4000))
  "Given an FFT analysis, apply the specified number of LOG mel filters (evenly spaced in the mel frequency domain(?)) between the start
   and end frequency values.
   Note: This gets called on every frame"
  (let ((mel-feature-values (get-mel-feature-values dft-analysis filter-bank-size :start-freq start-freq :end-freq end-freq)))
    (map-into mel-feature-values 'log mel-feature-values)
    mel-feature-values))

(defun mel-features->spectrogram (mel-feature-list)
  "This *tries* to go from a mel feature value list, back to the dft-analysis/spectrogram."
  (let ((spectrogram (make-array (* (length mel-feature-list) 10) :initial-element 0)))
    (dotimes (i (length mel-feature-list))
      (loop for j from 0 to 10 do 
	   (when (< (+ (* i 10) j) (* (length mel-feature-list) 10))
	     (incf (elt spectrogram (+ (* i 10) j))
		   (* (/ (- 10 j) 10)
		      (elt mel-feature-list i))))
	   (when (>= (- (* i 10) j) 0)
	     (incf (elt spectrogram (- (* i 10) j))
		   (* (/ (- 10 j) 10)
		      (elt mel-feature-list i))))))
    spectrogram))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; SIGNAL PROCESSING FUNCTIONS?!?!  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Signal processing functions")

(defun pad-sample-list-to-power-of-two (sample-list)
  "Zero-padding function."
  (declare (type simple-array sample-list))
  (let* ((length (length sample-list))
	 (power (1+ (floor (log length 2))))
	 (sample-list-length (expt 2 power))
	 (number-of-samples-added (- sample-list-length length))
	 (offset (floor (/ number-of-samples-added 2)))
	 (array (make-array (list sample-list-length) :initial-element 0.0 :element-type 'single-float )))
    (setf (subseq array offset (+ length offset)) sample-list)
    array))

(cl-user::todo "Try other windowing functions?")

(defun zero-pad-and-fft-audio-segment (audio)
  "Given some audio zero pad it up to the next power of two; compute DFT; and return the DFT analysis."
  (let* ((sample-list (audio-segment-samples audio))
	 (padded-sequence (pad-sample-list-to-power-of-two sample-list))
	 (dft-coefficients (bordeaux-fft:sfft padded-sequence))   ;;; we may have other options that we can use here!!
	 (coefficient-array (make-array (/ (length dft-coefficients) 2) :element-type 'single-float)))
    (map-into coefficient-array (lambda (x) (coerce (abs (the (complex double-float) x)) 'single-float)) dft-coefficients) ;; abs. value of the complex numbers... to get the magnitudes
    (map-into coefficient-array (lambda (x) (expt (the single-float x) 2)) coefficient-array) ;; take the square to get the power
    (make-dft-analysis :coefficient-list coefficient-array
		       :original-sampling-rate (truncate (audio-segment-sample-rate audio))) ;; convert it to an integer...
    ))
   
(defun dft-analysis->frequency-band-analysis (dft-analysis &key (bands nil)) ;;&key (bands 200))
  "Convert a DFT analysis into a spectrogram-style analysis."
  (unless bands
    (setf bands (/ (length (dft-analysis-coefficient-list dft-analysis)) 2)))
  (let* ((coefficient-list (map 'vector 'log (subseq (dft-analysis-coefficient-list dft-analysis)
						     0 (/ (length (dft-analysis-coefficient-list dft-analysis)) 2))))
	 (coefficient-count (length coefficient-list))
	 (band-width (floor (/ coefficient-count bands)))
	 (avg-values '())
	 (freq-ranges '()))
    (dotimes (i bands)
      (let* ((begin (* i band-width))
	     (end (+ begin band-width))
	     (this-band-values (subseq coefficient-list begin end))
	     (avg-value (/ (reduce '+ this-band-values) (length this-band-values)))
	     (bottom-freq  (get-frequency-of-fft-coefficient begin dft-analysis))
	     (top-freq  (get-frequency-of-fft-coefficient end dft-analysis)))
	(push avg-value avg-values)
	(push (list bottom-freq top-freq) freq-ranges)))
    (make-spectral-analysis :magnitude-list (nreverse avg-values)
			    :frequency-range-list (nreverse freq-ranges))))

(defun invert-list-array (list-array)
  "Given a 2D array, represented by lists, invert it.  This does a bunch of consing."
  (let ((new-list-array '())
	(tmp-list '()))
    (dotimes (column-num (length (first list-array))) ;; for each col num
      (dolist (row list-array)
	(push (elt row column-num) tmp-list))
      (push (nreverse tmp-list) new-list-array)
      (setf tmp-list '()))
    (nreverse new-list-array)))

(defun mel-features->mel-cepstrum-features (feature-list &optional (cepstrum-count 13))
  "Given a list of mel features, compute the DCT, then return the top-n of those."
  (declare (ignore cepstrum-count))
  ;;(dct feature-list cepstrum-count)
  (dct feature-list))

(defun mel-cepstrum-features->mel-features (feature-list &optional (n 40))
  "Convert a list of Mel cepstral feature values, back to mel features.  This is done by
   padding the cepstral feature values with zeros and performing an IDCT."
  (let ((new-list '()))
    (dotimes (i n)
      (push 0 new-list))
    (setf (subseq new-list 0 (length feature-list)) feature-list)    
    (idct new-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; MAIN FEATURE COMPUTATION  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Main feature computation functions")

;; This is the biggest offender w.r.t consing
(defun get-mel-cepstrum (audio &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13) (frame-length 25) (frame-delta 10))
  "Get mel cepstral features for an audio segment."
  (let* ((audio-frames (split-audio-into-frames audio :frame-length frame-length :frame-delta frame-delta))
	 (fft-analyses (mapcar 'zero-pad-and-fft-audio-segment audio-frames)) ;; <-- ~55% (22% is FFT)
	 (mel-feature-values (mapcar (lambda (x) (get-log-mel-feature-values x filter-bank-size :start-freq start-freq :end-freq end-freq)) fft-analyses)) ;; ~1.7% of consing.
	 (mel-cepstrum-features (mapcar (lambda (x) (mel-features->mel-cepstrum-features x cepstrum-count)) mel-feature-values))) ;; <-- 17% of consing (DCT)
    mel-cepstrum-features))

(defun get-list-of-length-n (item length)
  "Get a list of the specified length, containing the specified element."
  (make-list length :initial-element item))

(defun get-feature-deltas (feature-list-array)
  "Given a list of feature lists, compute the delta values for each feature."
  (let ((feature-count (length (first feature-list-array)))
	(delta-list '())
	(previous-feature-vector nil))
    (dolist (feature-vector feature-list-array)
      (if previous-feature-vector
	  (push (mapcar '- feature-vector previous-feature-vector)
		delta-list)
	  (push (get-list-of-length-n 0.0 feature-count) delta-list))
      (setf previous-feature-vector feature-vector))
    (nreverse delta-list)))

(defun get-mfcc-features (audio &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13) (preemphasize 0.95)
			  (cepstra-mean-normalization t) (frame-length 25) (frame-delta 10))
  "Get the complete MFCC feature vectors with velocity and acceleration values."
  (when preemphasize (setf audio (preemphasize-audio audio preemphasize)))
  (let* ((mfccs (get-mel-cepstrum audio 
				  :filter-bank-size filter-bank-size 
				  :start-freq start-freq
				  :end-freq end-freq
				  :cepstrum-count cepstrum-count
				  :frame-length frame-length
				  :frame-delta frame-delta ))
	 (xxx-ignore (when cepstra-mean-normalization
		       (setf mfccs (cepstra-mean-normalization mfccs))))
	 (velocity-features (get-feature-deltas mfccs))
	 (acceleration-features (get-feature-deltas velocity-features)))
    (declare (ignore xxx-ignore))
    (coerce (mapcar (lambda (x y z) (coerce (append x y z) '(vector single-float))) mfccs velocity-features acceleration-features)  'vector)))


(defun convert-audio-to-mfcc-file (filename &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13) (preemphasize 0.95) (cepstra-mean-normalization t) (frame-length 25) (frame-delta 10) (extension ".mfc") (filename-transcript-function nil) (transcript nil))
  "Given an audio filename, compute MFCC features for that audio and write them to a .mfc file."
  (let* ((audio (cond ((cl-ppcre:scan ".audio$" filename)
		       (load-audio-file filename))
		      ((cl-ppcre:scan ".wav$" filename)
		       (load-wav-file filename))
		      (t (error "Unknown audio file extension!"))))
	 (mfccs (get-mfcc-features audio :filter-bank-size filter-bank-size :start-freq start-freq :end-freq end-freq :cepstrum-count cepstrum-count :preemphasize preemphasize :cepstra-mean-normalization cepstra-mean-normalization :frame-length frame-length :frame-delta frame-delta))
	 (mfcc-struct (make-mfcc-sequence
		       :seconds (audio-segment-seconds audio)
		       :id (audio-segment-id audio)
		       :source (audio-segment-source audio)
		       :sample-rate (audio-segment-sample-rate audio)
		       :sample-resolution (audio-segment-sample-resolution audio)
		       :transcript (audio-segment-transcript audio)
		       :mfcc-seq mfccs
		       :filter-bank-size filter-bank-size
		       :start-freq start-freq
		       :end-freq end-freq
		       :cepstrum-count cepstrum-count
		       :preemphasis preemphasize
		       :cepstra-mean-normalization cepstra-mean-normalization
		       :frame-length frame-length
		       :frame-delta frame-delta))
	 (new-filename filename))
    (when filename-transcript-function
      (let ((transcript (funcall filename-transcript-function filename)))
	(setf (mfcc-sequence-transcript mfcc-struct) transcript)))
    (when transcript
      (setf (mfcc-sequence-transcript mfcc-struct) transcript))
    (setf new-filename (cl-ppcre:regex-replace ".[A-Za-z]*$" new-filename extension ))  ;;; removes the existing extension?
    (save-object mfcc-struct new-filename)
    (values)))

(defun ti-digit-filename->transcript (filename-string)
  "Automatically extract the transcripts for the Aurora dataset from the filename.
   This was simply to avoid writing code for reading in the transcripts separately."
  (let* ((relevant-name-string (subseq filename-string
				       (1+ (position #\_ filename-string :from-end t))
				       (1- (position #\. filename-string :from-end t))))
	 (digit-list (cl-ppcre:split "" relevant-name-string)))
    (setf digit-list (substitute "0" "Z" digit-list :test 'string-equal))
    (setf digit-list (substitute "oh" "O" digit-list :test 'string-equal))
    (format nil "~{~A~^ ~}" digit-list)))
  
