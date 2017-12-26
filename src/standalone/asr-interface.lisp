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

;;;; Main interface functions for ASR

;;;; TODO Large portions of this file are probably largely obsolete at this point.")

#+port-audio
(defun recognize-speech-live (seconds transcript &rest rest)
  "Record speech live, then immediately recognize it."
  (let* ((audio (record-audio :seconds seconds :transcript transcript))
	 (rec-result (apply 'recognize-speech audio rest)))
    (print-recognition-result rec-result)))

(defun recognize-speech (audio &key (relative-threshold nil) (preemphasize 0.95) (filter-bank-size 40) (start-freq 100)
			 (end-freq 4000) (cepstrum-count 13) (frame-length 25) (frame-delta 10)
			 grammar-filename
			 (retain-complete-bp-table t)
			 (show-bp-table nil)
			 (show-trellis nil)
			 (show-spectrogram nil)
			 (show-features nil)
			 (show-mel-cepstrum nil)
			 (show-ceptrum-with-hypothesis nil)
			 model-directory
			 (dictionary "./misc/dict/an4.dic")
			 (phone-or-word :word)
			 (word-insertion-penalty 200.0)
			 (silence-penalty 150.0)
			 (language-model "./misc/lm/an4.trigramlm")
			 (language-weight 9.5)
			 (use-semantics nil)
			 (semantic-weight 0.0))
  "A high-level function for performing recognition on audio.  Performs feature extraction before recognition."
  (when (or show-spectrogram show-features show-mel-cepstrum) (write-line "Displaying data...")(force-output t))
  (when show-spectrogram (show-spectrogram audio))
  (when show-features (show-complete-feature-set audio))
  (when show-mel-cepstrum (show-mel-cepstrum audio))
  (when (or show-spectrogram show-features show-mel-cepstrum) (write-line "Finished displaying data.")(force-output t))    
  (let* ((mfccs (cond ((typep audio 'audio-segment)
		       (get-mfcc-features audio :preemphasize preemphasize :filter-bank-size filter-bank-size :start-freq start-freq :end-freq end-freq
					  :cepstrum-count cepstrum-count :frame-length frame-length :frame-delta frame-delta))
		      ((typep audio 'mfcc-sequence)
		       (mfcc-sequence-mfcc-seq audio))
		      ((typep audio 'sequence)
		       audio)
		      (t (error "Unknown input audio type"))))
	 (transcript (cond ((and (typep audio 'audio-segment) (audio-segment-transcript audio))
			    (cl-ppcre:split " +" (audio-segment-transcript audio)))
			   ((and (typep audio 'mfcc-sequence) (mfcc-sequence-transcript audio))
			    (mfcc-sequence-transcript audio)))))
    (recognize-speech-from-mfccs mfccs :relative-threshold relative-threshold 
				 :grammar-filename grammar-filename
				 :retain-complete-bp-table retain-complete-bp-table
				 :show-bp-table show-bp-table
				 :show-trellis show-trellis
				 :show-ceptrum-with-hypothesis show-ceptrum-with-hypothesis
				 :model-directory model-directory
				 :transcript transcript
				 :dictionary dictionary
				 :phone-or-word phone-or-word
				 :word-insertion-penalty word-insertion-penalty
				 :silence-penalty silence-penalty
				 :language-model language-model
				 :language-weight language-weight
				 :semantic-weight semantic-weight
				 :use-semantics use-semantics)))

(defun recognize-speech-from-mfccs (mfccs &key 
				    grammar-filename
				    model-directory
				    (dictionary "./misc/dict/an4.dic")
				    (phone-or-word :word)
				    transcript
				    (relative-threshold nil)
				    (retain-complete-bp-table nil)
				    (show-bp-table nil)
				    (show-trellis nil)
				    (show-ceptrum-with-hypothesis nil)
				    (word-insertion-penalty 200.0)
				    (silence-penalty 150.0)
				    (language-model "./misc/lm/an4.trigramlm")
				    (language-weight 9.5)
				    (use-semantics nil)
				    (semantic-weight 0.0))
  "A relatively high-level function for recognition, but requires that feature extraction has already been performed."
  (let ((start-time (get-internal-real-time)))
    (unless model-directory (error "Model directory is required!"))
    (when dictionary (read-dictionary dictionary))    
    (let* ((lang-hmm (cond (grammar-filename
			    (load-grammar grammar-filename model-directory 
					  :model-source phone-or-word 
					  :word-insertion-penalty word-insertion-penalty 
					  :silence-penalty silence-penalty))
			   (language-model
			    (build-flat-bigram-lang-hmm-from-lm language-model model-directory 
								:model-source phone-or-word 
								:word-insertion-penalty word-insertion-penalty
								:silence-penalty silence-penalty))
			   (t (error "Must specify either a grammar file or a language model."))))
	   (rec-result (search-illusory-trellis-wrapper mfccs lang-hmm 
							:relative-threshold relative-threshold 
							:retain-complete-bp-table retain-complete-bp-table 
							:retain-complete-trellis show-trellis
							;;:insertion-penalty insertion-penalty
							:insertion-penalty (coerce 0.0 'single-float)
							:language-weight (coerce language-weight 'single-float)
							:use-lm (and language-model (not grammar-filename))
							:use-semantics use-semantics
							:semantic-weight (coerce semantic-weight 'single-float))))
      (when transcript
	(setf (rr-reference rec-result) transcript))
      (compute-rr-wer rec-result)
      (when show-ceptrum-with-hypothesis (show-mel-cepstrum-with-hypothesis mfccs (rr-phoneme-path rec-result)))
      (when show-trellis (print-trellis-new (rr-complete-trellis rec-result)))
      (when show-bp-table (print-trellis-new (rr-complete-bp-table rec-result)))
      (let* ((end-time (get-internal-real-time))
	     (run-time (/ (- end-time start-time) 1000))
	     (real-time-factor (if (typep mfccs 'audio-segment)
				   (/ run-time (audio-segment-seconds mfccs))
				   0.0)))
	(setf (rr-total-time rec-result) (float run-time))
	(setf (rr-real-time-factor rec-result) real-time-factor)
	rec-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Evaluation - evaluate models against reference transcriptions  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-on-test-set (directory-name ctl-file verbose &rest rest)
  "Run the recognizer on all the files in a directory and print some info about the results.
   Wants .mfc files only."
  (let ((files (if ctl-file
		   (file->line-list ctl-file)
		   (mapcar 'file-namestring (list-directory directory-name))))
	(start-time (get-internal-real-time))
	(example-count 0)
	(word-count 0)
	(error-count 0))
    (unless ctl-file
      (setf files (remove-if-not (lambda (x) (cl-ppcre:scan ".mfc$" x)) files)))
    (dolist (file files)
      (when (cl-ppcre:scan ".wav$" file)
	(setf file (cl-ppcre:regex-replace ".wav$" file ".mfc")))
      (let* ((audio (load-mfcc-audio (format nil "~A/~A" directory-name file)))
	     (result (apply 'recognize-speech audio rest )))
	(when verbose
	  (format t "----------------------------------------------~%")
	  (format t "File name: ~A~%" file)
	  (format t "Reference:  ~{~A ~}~%" (rr-reference result))
	  (format t "Hypothesis: ~{~A ~} [WER: ~,2f]~%" (rr-hypothesis result) (rr-wer result))(force-output t))
	(incf word-count (rr-reference-length result))
	(incf error-count (rr-errors result))
	(incf example-count)))
    (let* ((end-time (get-internal-real-time))
	   (total-time (/ (- end-time start-time) 1000))
	   (avg-time (/ total-time example-count)))
      (format t "Average WER: ~3,2F % [~A/~A] [Avg time: ~3,3f s]~%" (* 100 (/ error-count word-count)) error-count word-count avg-time))))
