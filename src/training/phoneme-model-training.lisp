;;;; Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Create initial models     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smush-list (list)
  "Append a list of strings, remove duplicates, and then sort and return the remaining strings."
  (setf list (apply 'append list))
  (setf list (remove-duplicates list :test 'string-equal))
  (setf list (sort list 'string-lessp))
  list)

(defun create-initial-phoneme-models (training-data-dir new-model-dir dictionary-filename iterations &key (state-count 3) (pad-with-silence t))
  "Bootstrap the phoneme models."
  (when pad-with-silence
    (setf pad-with-silence "SIL"))

  (read-dictionary dictionary-filename)
  (let ((files (list-directory training-data-dir))
	(examples nil)
	(word-list '())
	(phoneme-list '(("SIL"))))
    (setf files (remove-if-not (lambda (x) (cl-ppcre:scan ".mfc$" x)) files)) ;; mfc files only...
    (format t "Preparing to train from ~A examples.~%" (length files))
    (format t "Loading training data...~%") (force-output t)
    (setf examples (make-array (length files) :fill-pointer 0 :element-type 'connected-training-example))
    (dolist (file files)
      (let* ((mfc-obj (load-mfcc-audio (format nil "~A/~A" training-data-dir file)))
	     (example (make-connected-training-example :features mfc-obj :lang-hmm nil))
	     (transcript (mfcc-sequence-transcript mfc-obj)))
	(push transcript word-list)
	(push (word-sequence->phoneme-sequence transcript) phoneme-list)
	(vector-push example examples)))
    (format t "Loaded ~A training files.~%" (length examples))
    (setf word-list (smush-list word-list))
    (format t "Data includes the following ~A word: ~{~A ~}~%" (length word-list) word-list)
    (setf phoneme-list (smush-list phoneme-list))
    (format t "Training the following ~A phonemes: ~{~A ~}.~%" (length phoneme-list) phoneme-list)

    ;; Do the initial segmentation uniformly
    (segment-phoneme-training-examples-uniformly examples :state-count state-count :pad-with-silence pad-with-silence)

    (let ((current-models (retrain-models-from-segmented-examples examples phoneme-list nil state-count)))
      (dotimes (i iterations)
	(incorporate-new-phoneme-models-into-examples-for-bootstrapping examples current-models :pad-with-silence pad-with-silence)
	(format t "Segmenting training data...~%") (force-output t)
	(segment-training-examples examples)
	(format t "Retraining models...~%") (force-output t)
	(setf current-models (retrain-models-from-segmented-examples examples phoneme-list current-models state-count))) ;; returns a new model table
      (write-models-to-folder current-models new-model-dir))))

(defun segment-phoneme-training-examples-uniformly (examples &key (state-count 3) (pad-with-silence nil) debug)  ;;; the word models are included in the "examples" structures
  "Given some training examples, segment them uniformly."
  (declare ((vector connected-training-example) examples))
  (loop for example across examples do
       
       (let* ((mfc (connected-training-example-features example))
	      (raw-phoneme-seq (word-sequence->phoneme-sequence (if (stringp (mfcc-sequence-transcript mfc))
								    (cl-ppcre:split " +" (mfcc-sequence-transcript mfc))
								    (mfcc-sequence-transcript mfc))))
	      (phoneme-seq (if pad-with-silence (append (list "SIL") raw-phoneme-seq (list "SIL")) raw-phoneme-seq)))
	 (unless phoneme-seq (format t "No phonemes found in the dictionary for example: ~A." (mfcc-sequence-filename mfc)))
	 (when phoneme-seq
	   (when debug (format t "Doing uniform segmentation with these phonemes: ~{~A ~}.~%" phoneme-seq))
	   ;; First figure out long each segment should be, how many, etc.
	   (let* ((segmentation '())
		  (total-segments (* (length phoneme-seq) state-count))
		  (total-frames (length (mfcc-sequence-mfcc-seq mfc)))
		  (segment-length (floor (/ total-frames total-segments)))
		  (remainder-length (- total-frames (* segment-length total-segments))))
	     (when debug (format t "Uniform segment length: ~A, Length of final segment: ~A, total frame count: ~A~%" segment-length remainder-length total-frames))
	     ;; Segment uniformly for all the phonemes
	     (loop for phoneme in phoneme-seq do
		  (loop for i from 0 below state-count do
		       (dotimes (n segment-length)
			 (push (list phoneme i) segmentation))))
	     ;; Since the number of frames may not properly be divisible by the number of phonemes/states,
	     ;; we give the last phoneme the extra frames
	     (let ((last-phoneme (elt phoneme-seq (1- (length phoneme-seq)))))
	       (dotimes (i remainder-length)
		 (push (list last-phoneme (1- state-count)) segmentation)))
	     ;; Put it back in the correct order, and save the uniform segmentation in the 'example' struct
	     (setf segmentation (nreverse segmentation))
	     (setf (connected-training-example-segmentation example) segmentation))))))

(defun incorporate-new-phoneme-models-into-examples-for-bootstrapping (examples new-models &key (pad-with-silence nil) silence-penalty)
  "Given an array of examples, re-build each example with a new set of word models."
  (loop for i from 0 below (length examples)
     for example = (elt examples i) do
       (let* ((features (connected-training-example-features example))  ;; this is where we incorporate the new models...
	      (new-lang-hmm (compose-phoneme-hmm-for-training-example-for-bootstrapping (connected-training-example-features example) new-models 
											:pad-with-silence pad-with-silence
											:silence-penalty silence-penalty))
	      (new-example (make-connected-training-example :features features
							    :lang-hmm new-lang-hmm)))
	 (setf (aref examples i) new-example))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Main phoneme training     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun train-phoneme-models (training-data-dir initial-model-dir new-model-dir iterations dictionary-filename
			     &key (save-intermediate-models nil)
			     (evaluate-intermediate-models nil)
			     (evaluation-data-folder nil)
			     (state-count 3)
			     (file-list nil)
			     (pad-with-silence t)
			     silence-penalty)
  "This is the main training from connected speech function."
  (read-dictionary dictionary-filename)
  (let ((files (if file-list 
		   file-list 
		   (list-directory training-data-dir)))
	(examples nil)
	(phoneme-list '())
	(initial-models (get-initial-word-hmms initial-model-dir)))

    (unless file-list
      (setf files (remove-if-not (lambda (x) (cl-ppcre:scan ".mfc$" x)) files))) ;; mfc files only...

    (format t "Preparing to train from ~A examples.~%" (length files))
    (format t "Loading training data...~%") (force-output t)
    (setf examples (make-array (length files) :fill-pointer 0 :element-type 'connected-training-example))
    (dolist (file files)
      (when (cl-ppcre:scan ".wav$" file) ;; if there's a ctl file, it probably lists .wav files... look for .mfc instead
	(setf file (cl-ppcre:regex-replace ".wav$" file ".mfc")))
      (let* ((mfc-obj (load-mfcc-audio (format nil "~A/~A" training-data-dir file)))
	     (lang-hmm (compose-phoneme-hmm-for-training-example mfc-obj initial-models
								 :pad-with-silence pad-with-silence 
								 :silence-penalty silence-penalty
								 :insert-optional-silences-between-words t)) ;; the counter-part of: 'compose-hmm-for-training-example
	     (example (make-connected-training-example :features mfc-obj
						       :lang-hmm lang-hmm))
	     (transcript (mfcc-sequence-transcript mfc-obj)))
	(push (word-sequence->phoneme-sequence transcript)
	      phoneme-list)
	(vector-push example examples)))
    (setf phoneme-list (smush-phoneme-list phoneme-list))
    (format t "Training the following ~A phonemes: ~{~A ~}.~%" (length phoneme-list) phoneme-list)
    (format t "Loaded ~A training files.~%" (length examples))
    (let ((current-models initial-models))
      (dotimes (i iterations)
	(format t "Segmenting training data...~%") (force-output t)
	(segment-training-examples examples :segment-unit :phone)
	(format t "Retraining models...~%") (force-output t)
	(setf current-models (retrain-models-from-segmented-examples examples phoneme-list current-models state-count))
	(incorporate-new-phoneme-models-into-examples examples current-models :pad-with-silence pad-with-silence :silence-penalty silence-penalty)
	(when save-intermediate-models
	  (write-models-to-folder current-models (format nil "~A-iter~A" new-model-dir i)))
	(when evaluate-intermediate-models
	  (assert (probe-file evaluation-data-folder))
	  (assert save-intermediate-models)
	  (format t "Evaluation on iteration #~A:~%" i)
	  (evaluate-on-test-set evaluation-data-folder nil :model-directory (format nil "~A-iter~A" new-model-dir i) :grammar-filename "./grammars/digits.fsm")))
      (write-models-to-folder current-models new-model-dir))))

(defun incorporate-new-phoneme-models-into-examples (examples new-models &key (pad-with-silence nil) silence-penalty)
  "Given an array of examples, re-build each example with a new set of word models."
  (loop for i from 0 below (length examples)
     for example = (elt examples i) do
       (let* ((features (connected-training-example-features example))  ;; this is where we incorporate the new models...
	      (new-lang-hmm (compose-phoneme-hmm-for-training-example (connected-training-example-features example) new-models
								      :pad-with-silence pad-with-silence
								      :silence-penalty silence-penalty))
	      (new-example (make-connected-training-example :features features
							    :lang-hmm new-lang-hmm)))
	 (setf (aref examples i) new-example))))








