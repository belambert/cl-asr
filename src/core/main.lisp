;;;; Author: Ben Lambert
;;;; ben@benjaminlambert

(in-package :sphinx-l)

;;;; Contains the top-level decoding functions

(pushnew :sphinx-l *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  Speech Recognition (a.k.a decoding)  ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun string->words (string)
  "Split a string by spaces into words, and add a silence at the beginning and end.
   This is used when aligning a string to audio, so we don't have to split the string into
   words in advance.  The silences are also important since most audio recording will have leading
   and trailing silence."
  (let ((words (split-sequence:split-sequence #\Space string)))
    words))

(defun print-match-scores (&key mfcc-file output-file log-base)
  (let* ((mfccs (read-mfcc-file mfcc-file)))
    (with-open-file (f output-file :direction :output :if-exists :supersede)
      (loop for feature-vector across mfccs do	 
	   (loop for gmm across (acoustic-model-gmms *acoustic-model*)
	      for match-score = (log-gaussian-mixture-probability feature-vector gmm) do
		(when log-base
		  (setf match-score (/ match-score (the single-float (log (the single-float log-base))))))
		(format f "~F " match-score))
	   (terpri f)))))

(defun list-modes ()
  (list :fsm
	:align
	:flat-ci  
	:flat-cd-basic
	:flat-cd-both-basic
	:flat-cd
	:flat-cd-both
	:lextree-ci
	:lextree-cd-basic
	:lextree-cd))

;; TODO - add the option to use a finite state grammar as the LM .... load this by calling: READ-FINITE-STATE-GRAMMAR or LOAD-GRAMMAR
(defun decode (mfcc-file &key
	       ;;model-directory
	       ;;dictionary
	       ;;language-model
	       (language-weight 9.5)
	       (relative-threshold nil)
	       (beam-threshold nil)
	       (max-width nil)
	       (retain-complete-bp-table nil)
	       (show-bp-table nil)
	       (show-trellis nil)
	       (show-spectrum-with-hypothesis nil)
	       insertion-penalty
	       insertion-probability
	       silence-penalty
	       silence-probability
	       (compare-function #'log-gaussian-mixture-probability)
	       align-reference
	       spectrum-filename-suffix
	       phoneme-recognition
	       ci
	       verbose
	       (use-original-mfccs-in-spectrogram t)
	       (spectrogram-graph-type :aqua)
	       log-base
	       (mode :lextree-cd)  ;; flat or lextree or lextree-cd...
	       (recognizep t)
	       (graph-language-hmm nil)
	       vocab)
  "A relatively high-level function for recognition, but requires that feature extraction has already been performed."
  (assert (bl::xor insertion-penalty insertion-probability))
  (assert (bl::xor silence-penalty silence-probability))
  (assert *lm*)
  (assert *acoustic-model*)

  (when silence-probability (setf silence-penalty (log silence-probability log-base)))
  (when insertion-probability (setf insertion-penalty (log insertion-probability log-base)))

  (format t "Relative threshold: ~A~%" relative-threshold)
  (format t "Beam threshold:     ~A~%" beam-threshold)
  (format t "Insertion penalty:  ~A~%" insertion-penalty)
  (format t "Silence penalty:    ~A~%" silence-penalty)

  (let ((start-time (get-internal-real-time)))
    ;;(load-models :model-directory model-directory :dictionary dictionary :language-model language-model)
    (alexandria:coercef insertion-penalty 'single-float)
    (alexandria:coercef silence-penalty 'single-float)
    (alexandria:coercef log-base 'single-float)

    (when (stringp align-reference)
      (setf align-reference (string->words align-reference))
      (pprint align-reference))
    (when align-reference
      (setf mode :align))

    ;; The MFCCs, as is, don't work for recreating the spectrogram b/c we'd have to invert the transformation (if applicable),
    ;; and then remove the velocity and acceleration features.
    (let* ((mfccs (read-mfcc-file mfcc-file))
	   (raw-mfccs (read-raw-mfccs mfcc-file)) ;; we use these to if/when we have to show a spectrogram
	   (lm-vocab (coerce (language-model::vocab *lm*) 'list))
	   ;;(vocab (remove-if-not (lambda (x) (get-phonemes-for-word x)) (coerce (language-model::vocab *lm*) 'list)))
	   (dict-vocab (get-dict-vocab))
	   (full-vocab (if vocab
			   vocab
			   (sort (bl:intersection-fast lm-vocab dict-vocab :ht-test 'equalp) 'string-lessp)))
	   (vocab (remove-if-not (lambda (x) (get-phonemes-for-word x)) full-vocab))
	   (lang-hmm (case mode
		       (:fsm                (create-fsm-and-language-hmm-from-fsm full-vocab *acoustic-model* :ci ci :phoneme-recognition phoneme-recognition :align-reference align-reference))
		       (:align              (build-align-language-hmm align-reference *acoustic-model* log-base))
		       (:flat-ci            (create-flat-cd-language-hmm-basic full-vocab *acoustic-model* log-base :left-ci t :right-ci t))
		       ;; The basic loopback
		       (:flat-cd-basic      (create-flat-cd-language-hmm-basic full-vocab *acoustic-model* log-base :right-ci t))
		       (:flat-cd-both-basic (create-flat-cd-language-hmm-basic full-vocab *acoustic-model* log-base))		       
		       ;; Constrained loopback
		       (:flat-cd            (create-flat-cd-language-hmm full-vocab *acoustic-model* log-base :left-ci nil :right-ci t :loop-back :basic))
		       (:flat-cd-both       (create-flat-cd-language-hmm full-vocab *acoustic-model* log-base :left-ci nil :right-ci nil :loop-back :basic))		       
		       ;; The available flavors of lextree decoding
		       (:lextree-ci         (build-ci-lextree-hmm full-vocab *acoustic-model* log-base))
		       (:lextree-cd-basic   (build-cd-lextree-basic full-vocab *acoustic-model* log-base))
		       (:lextree-cd         (build-cd-lextree full-vocab *acoustic-model* log-base :left-ci nil :right-ci nil))
		       (otherwise (error "Unknown mode: ~A" mode))))
	   (dummy (print-language-hmm-summary lang-hmm))
	   (rec-result (when recognizep (search-illusory-trellis mfccs lang-hmm
								 :compare-function compare-function
								 :relative-threshold relative-threshold
								 :beam-threshold beam-threshold
								 :max-width max-width
								 :retain-complete-bp-table retain-complete-bp-table
								 :retain-complete-trellis show-trellis
								 :insertion-penalty (coerce insertion-penalty 'single-float)
								 :language-weight (coerce language-weight 'single-float)
								 :use-lm t
								 :retain-complete-bp-table nil
								 :verbose verbose
								 :log-base log-base))))
	   (declare (ignore dummy))
	   (when graph-language-hmm
	     (language-hmm->dot-file lang-hmm "language-hmm.dot"))

      (format t "VOCAB SIZE: ~:D~%" (length vocab))
      (when recognizep
	;; We're not keeping track of the phoneme boundaries, so we can't graph that.
	(when show-spectrum-with-hypothesis 
	  (let ((spectrum-filename (if spectrum-filename-suffix
				       (concatenate 'string (basename mfcc-file) spectrum-filename-suffix)
				       (basename mfcc-file)))
		(title (format nil "Spectrum (from cepstrum) with hypothesis for '~A'" (basename mfcc-file))))
	    (if use-original-mfccs-in-spectrogram
		(show-spectrum-with-hypothesis raw-mfccs (rr-bp-list rec-result) :filename spectrum-filename :title title :type spectrogram-graph-type)
		(show-spectrum-with-hypothesis (unextract-mfcc-features mfccs) (rr-bp-list rec-result) :filename spectrum-filename :title title :type spectrogram-graph-type))))
	(when show-trellis (print-trellis-new (rr-complete-trellis rec-result)))
	(when show-bp-table (print-trellis-new (rr-complete-bp-table rec-result)))
	(let* ((end-time (get-internal-real-time))
	       (run-time (/ (- end-time start-time) 1000))
	       (audio-duration (/ (length mfccs) 100))
	       (real-time-factor (* 1.0 (/ run-time audio-duration))))
	  (setf (rr-total-time rec-result) (float run-time))
	  (setf (rr-real-time-factor rec-result) real-time-factor))	
	;; After we're done, do a full GC since we've probably left tons of back-pointers and other garbage around
	(sb-ext:gc :full t)
	rec-result))))
  
(defun align (mfcc-file reference &rest rest)
  "Takes all the same arguments as DECODE above, but takes the aligned reference as a required non-keyword argument."
  (apply 'decode mfcc-file :align-reference reference rest))

(defun display-mfcc-file (filename &key (feature-count 13) (type :aqua))
  "Given an MFCC file, read it, convert it into spectra, and "
  (let* ((cepstra (read-raw-mfccs filename :feature-count feature-count))
	 (spectra (cepstra->spectra cepstra))
	 (frames (length (first spectra)))
	 (freq-bands (length spectra)))
    (gnuplot:PLOT-GRAPH (invert-list-array spectra) :heatmap
			:filename (basename filename)
			:height (* freq-bands 2)
			:width (* frames 5)
			:title (format nil "Spectrogram (from cepstra) for '~A'." (basename filename))
			:type type)))

(defun mfcc-file->samples (filename &key (feature-count 13))
  "Given an MFCC file, convert it to spectra, then convert the spectra to playable samples! (DOES NOT WORK YET!)"
  (let* ((cepstra (read-raw-mfccs filename :feature-count feature-count))
	 (spectra (mapcar (lambda (x) (coerce (mapcar 'exp x) 'vector)) (cepstra->spectra cepstra)))	 
	 ;; err, we also need to run the data in reverse through the mel filters...
	 ;; we need the "short time inverse FFT" here instead?
	 (sample-vectors-complex (mapcar 'bordeaux-fft:sifft spectra))
	 (samples-complex (apply 'concatenate 'vector sample-vectors-complex))
	 (samples-real (map-into samples-complex 'realpart samples-complex)))
    samples-real))

#+port-audio
(defun play-samples (samples &key (sample-rate 12800))
  "Given an audio-segment, play it back."
  (handler-case
      (port-audio:write-audio-samples samples :sample-rate sample-rate)
    (error (e) (format t "Playback error: ~A~%" e))))

#+port-audio
(defun play-mfcc-file (filename)
  "Given an MFCC file, convert it to spectra, then convert the spectra to playable samples, and play the samples! (DOES NOT WORK YET!)"
  (format t "Converting file to samples...~%")
  (let ((samples (mfcc-file->samples filename)))
    (format t "Playing samples...~%")
    (play-samples samples)))

(defun mfcc-file->wav-file (mfcc-filename wav-file)
  "Given an MFCC file, convert it to spectra, then convert the spectra to playable samples, and save the samples to a WAV file! (DOES NOT WORK YET!)"
  (format t "Converting file to samples...~%")
  (let ((samples (mfcc-file->samples mfcc-filename)))
    (format t "Playing samples...~%")
    (save-wav-file wav-file samples :verbose t :sample-rate 12800)))
