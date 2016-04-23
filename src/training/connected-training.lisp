;;;; Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Main training      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct connected-training-example
  "An audio training example.  This consists of both some audio (i.e. mfcc sequence),
   a language HMM corresponding to transcript of that audio, and later a segmentation
   of the audio according to the language HMM model."
  (features nil :type mfcc-sequence)
  (lang-hmm nil :type (or language-hmm null))
  segmentation)

;;; TODO This function is really gnarly, e.g. has a fixed vocab, etc.
;;; TODO Also, rename this function, it's not just for digits anymore.
;;; TODO This is almost identical to the training function for training phoneme models

(defun train-from-connected-words (training-data-dir initial-model-dir new-model-dir iterations 
				   &key (save-intermediate-models nil)
				   (evaluate-intermediate-models nil)
				   (evaluation-data-folder nil)
				   (pruning-threshold nil)
				   (state-count 5)
				   (silence-penalty 100.0))
  "This is the main training from connected speech function."
  (let ((files (remove-if-not (lambda (x) (cl-ppcre:scan ".mfc$" x))
			      (list-directory training-data-dir)))
	(examples nil)
	(vocab-list '())
	(initial-word-models (when initial-model-dir (get-initial-word-hmms initial-model-dir))))
    (format t "Preparing to train from ~A examples.~%" (length files))
    (format t "Loading training data and composing language HMMs for each...~%") (force-output t)
    (setf examples (make-array (length files) :fill-pointer 0 :element-type 'connected-training-example))
    (dolist (file files)
      (let* ((mfc-obj (load-mfcc-audio (format nil "~A/~A" training-data-dir file)))
	     (lang-hmm (if initial-word-models
			   (compose-hmm-for-training-example mfc-obj initial-word-models :silence-penalty silence-penalty)
			   nil))
	     (example (make-connected-training-example :features mfc-obj
						       :lang-hmm lang-hmm))
	     (transcript (mfcc-sequence-transcript mfc-obj)))
	(push transcript vocab-list)
	(vector-push example examples)))

    ;; Setup the vocab list
    (setf vocab-list (apply 'append vocab-list))
    (push "<sil>" vocab-list)
    (setf vocab-list (remove-duplicates vocab-list :test 'string-equal))
    (setf vocab-list (sort vocab-list 'string-lessp))
    (format t "Training for vocabulary: ~{~A ~}.~%" vocab-list)
    (format t "Loaded ~A training files.~%" (length examples))
    ;;(setf vocab-list '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "oh" "<sil>"))
    ;; If initial models were not specifed, then create initial models with a uniform segmentation
    (unless initial-word-models
      (format t "WARNING: CREATING INITIAL MODELS WITH A UNIFORM SEGMENTATION.~%")
      (segment-word-training-examples-uniformly examples :state-count state-count :pad-with-silence t)
      (setf initial-word-models
	    (retrain-models-from-segmented-examples examples vocab-list nil state-count))
      (incorporate-new-models-into-examples examples initial-word-models :silence-penalty silence-penalty)
      (format t "Finished training initial models with uniform segmentation.~%"))

    (let ((current-models initial-word-models))
      (dotimes (i iterations)
	(format t "Segmenting training data...~%") (force-output t)
	(segment-training-examples examples :pruning-threshold pruning-threshold)
	(format t "Retraining models...~%") (force-output t)
	(setf current-models
	      (retrain-models-from-segmented-examples examples vocab-list current-models state-count))
	(incorporate-new-models-into-examples examples current-models :silence-penalty silence-penalty)
	(when save-intermediate-models
	  (write-models-to-folder current-models (format nil "~A-iter~A" new-model-dir i)))
	(when evaluate-intermediate-models
	  (assert (probe-file evaluation-data-folder))
	  (assert save-intermediate-models)
	  (format t "Evaluation on iteration #~A:~%" i)
	  (evaluate-on-test-set evaluation-data-folder nil :model-directory (format nil "~A-iter~A" new-model-dir i) :grammar-filename "./misc/rammars/digits.fsm")))
      (write-models-to-folder current-models new-model-dir))))

(defun segment-word-training-examples-uniformly (examples &key (state-count 5) (pad-with-silence t) debug)
  "Given some training examples, segment them uniformly.
   The word models are included in the 'examples' structures"
  (declare ((vector connected-training-example) examples))
  (loop for example across examples do       
       (let* ((mfc (connected-training-example-features example))
	      (word-seq  (if (stringp (mfcc-sequence-transcript mfc))
			     (cl-ppcre:split " +" (mfcc-sequence-transcript mfc))
			     (mfcc-sequence-transcript mfc))))
	 (assert word-seq)
	 (when pad-with-silence 
	   (setf word-seq (append (list "<sil>") word-seq (list "<sil>"))))
	 ;; First figure out long each segment should be, how many, etc.
	 (let* ((segmentation '())
		(total-segments (* (length word-seq) state-count))
		(total-frames (length (mfcc-sequence-mfcc-seq mfc)))
		(segment-length (floor (/ total-frames total-segments)))
		(remainder-length (- total-frames (* segment-length total-segments))))
	   (when debug (format t "Uniform segment length: ~A, Length of final segment: ~A.~%" segment-length remainder-length))
	   ;; Segment uniformly for the first n-1 segements
	   (loop for word in word-seq do
		(loop for i from 0 below state-count do
		     (dotimes (n segment-length)
		       (push (list word i) segmentation))))
	   ;; Since the number of frames may not properly be divisible by the number of phonemes/states,
	   ;; we may "short change" the last one.
	   (let ((last-word (elt word-seq (1- (length word-seq)))))
	     (dotimes (i remainder-length)
	       (push (list last-word (1- state-count)) segmentation)))
	     ;; Put it back in the correct order, and save the uniform segmentation in the 'example' struct
	   (setf segmentation (nreverse segmentation))
	   (setf (connected-training-example-segmentation example) segmentation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Training helper functions    ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-initial-word-hmms (hmm-directory)
  "Given a folder name, read all the .hmm files into a hash table and return
   the hashtable.  They keys of the hashtable are the strings for the words."
  (let ((filenames (list-directory hmm-directory))
	(hmm-table (make-hash-table :test 'equalp)))
    (dolist (filename filenames)
      (when (cl-ppcre:scan ".hmm$" filename)
	(let ((hmm (load-hmm (format nil "~A/~A" hmm-directory filename))))
	  (setf (gethash (hmm-word hmm) hmm-table) hmm))))
    hmm-table))

(defun compose-hmm-for-training-example (mfcc-seq word-hmms &key silence-penalty)
  "Given some audio and a hash-table of word-hmms, construct a simple language 
   HMM to that may be used for training/segmentation."
  (let* ((word-seq (mfcc-sequence-transcript mfcc-seq))
	 (fsm (create-fsm-from-word-seq word-seq))
	 (lang-hmm (create-language-hmm fsm word-hmms :model-source :word :silence-penalty silence-penalty )))
    lang-hmm))

(defun incorporate-new-models-into-examples (examples new-models &key silence-penalty)
  "Given an array of examples, re-build each example with a new set of word models."
  (loop for i from 0 below (length examples)
       for example = (elt examples i) do
       (let* ((features (connected-training-example-features example))
	      (new-lang-hmm (compose-hmm-for-training-example (connected-training-example-features example) new-models :silence-penalty silence-penalty))
	      (new-example (make-connected-training-example :features features :lang-hmm new-lang-hmm)))
	 (setf (aref examples i) new-example))))

(defun make-new-training-example (mfc-obj word-models &key silence-penalty)
  "Make a new training example, with the new word models?
   This never gets called?!?!?"
  (make-connected-training-example :features mfc-obj :lang-hmm (compose-hmm-for-training-example mfc-obj word-models :silence-penalty silence-penalty)))

(defun write-models-to-folder (models folder)
  "Write word HMM models to a folder."
  (loop for word being the hash-keys of models do
       (let ((hmm (gethash word models))
	     (filename (format nil "~A/~A.hmm" folder word)))
	 (save-hmm hmm filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Segmentation of training data  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun segment-audio-with-lang-hmm (mfc hmm &key (pruning-threshold nil) (segment-unit :word) (display-segmentation nil) debug)
  "Segment audio with a language HMM."
  (let* ((fv-seq (mfcc-sequence-mfcc-seq mfc))
	 (result (search-illusory-trellis fv-seq hmm :relative-threshold pruning-threshold))
	 (word-hmm-states (cond ((eq segment-unit :word)
				 (get-word-hmm-states (rr-shortest-path result) hmm))
				((eq segment-unit :phone)
				 (get-phoneme-hmm-states (rr-shortest-path result) hmm))
				(t (error "Invalid segment unit.  Must be :phone or :word."))))
	 (likelihood (rr-best-path-value result)))
    (when debug
      (format t "Segmentation: ~{~A ~}~%" (rr-best result))
      (format t "Phoneme path: ~{~A ~}~%" (rr-phoneme-path result))
      (format t "Word path: ~{~A~^ ~}~%" (rr-word-path result))(force-output))
    (when display-segmentation
      (show-mel-cepstrum-with-hypothesis fv-seq (rr-word-path result)))
    (values word-hmm-states likelihood)))

(defun get-word-hmm-states (path hmm)
  "From a shortest trellis path, return a sequence of word-hmm states (e.g. word='one', state='3')"
  (let ((word-state-map (language-hmm-word-state-map hmm))
	(state-path '()))
    (dolist (state path)
      (push (elt word-state-map state)
	    state-path))
    (nreverse state-path)))

(defun get-phoneme-hmm-states (path hmm)
  "From a shortest trellis path, return a sequence of phoneme-hmm states (e.g. phoneme='AX', state='3')"
  (let ((phoneme-state-map (language-hmm-phoneme-state-map hmm))
	(state-path '()))
    (dolist (state path)
      (push (elt phoneme-state-map state)
	    state-path))
    (nreverse state-path)))

(defun segment-training-examples (examples &key (pruning-threshold nil) (segment-unit :word))
  "Given some training examples, segment them.
   The word models are included in the 'examples' structures"
  (declare ((vector connected-training-example) examples))
  (let ((likelihood-sum 0.0)
	(example-count 0))
    (declare (single-float likelihood-sum))
    (loop for example across examples do
      (let* ((mfc (connected-training-example-features example))
	     (hmm (connected-training-example-lang-hmm example)))
	(incf example-count)
	    (multiple-value-bind (segmentation likelihood)
		(segment-audio-with-lang-hmm mfc hmm :pruning-threshold pruning-threshold :segment-unit segment-unit)
	      (declare (single-float likelihood))
	      (setf (connected-training-example-segmentation example) segmentation)
	      (incf likelihood-sum likelihood))))
    (format t "Log likelihood: ~f~%" (- likelihood-sum))(force-output t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Core training/model update methods     ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun retrain-models-from-segmented-examples (examples vocab-list previous-models &optional (state-count 5))
  "Given some segmented example audio, re-train the word models."
  (let ((model-table (make-hash-table :test 'equalp)))
    (dolist (word vocab-list)
      (let* ((training-data (get-training-examples-for-word examples word state-count))
	     (new-model (retrain-word-model training-data word (length examples) state-count)))
	(if new-model ;; if we have a new model, save it.
	    (setf (gethash word model-table) new-model)
	    (progn ;; if we don't get a new model, use the previous one...
	      (when previous-models (setf (gethash word model-table) (gethash word previous-models)))
	      (format t "No new models for word '~A' this iteration... re-using previous model.~%" word)
	      ))))
    model-table))

(defun get-training-examples-for-word (examples word &optional (state-count 5) debug)
  "Get raw state training examples for the specified word.  This returns a vector with one entry per word HMM state.
   Each of the entries contains raw MFCC vectors, to be used in training the next version of the model."
  ;; this is an array that will hold the training data for each of the 
  (let ((vector-groups (make-array state-count :element-type '(or null (vector (vector single-float))) :initial-element nil)))
    (loop for example across examples do ;; look at each example..
      (let ((fv-list (mfcc-sequence-mfcc-seq (connected-training-example-features example))) ;;raw mfccs
	    (segmentation (connected-training-example-segmentation example))) ;; this is the segmentation...
	(when debug (format t "~{ ~A ~}~%" segmentation))
	(loop for segmentation-element in segmentation  ;; look at every mfcc vector
	   for i from 0 to (- (length segmentation) 2) do
	     (let ((this-vector (elt fv-list i))
		   (this-word (first segmentation-element))
		   (state-num (second segmentation-element)))
	       (when (string-equal this-word word) ;; if this mfcc corresponds to the word we're looking for, then save it.
		 (unless (aref vector-groups state-num)
		   (setf (aref vector-groups state-num) (make-array 10 :adjustable t :fill-pointer 0)))  ;; needs to be adjustable b/c we need as many as we have training examples
		 (vector-push-extend this-vector (aref vector-groups state-num)))))))
    (format t "State training examples for word '~A': ~{~A ~}~%" word (map 'list 'length vector-groups))
    vector-groups))

(defun retrain-word-model (training-data word example-count &optional (state-count 5))
  "Given the word-specific training data, build a new model for the word."
  (when (some (lambda (x) (= (length x) 0)) training-data)
    (format t "No training data for word '~A' on this iteration.~%" word)
    (return-from retrain-word-model nil))
  (let ((state-gaussians '())
	(state-ids '()))
    (dotimes (i state-count)
      (push word state-ids))
    (loop for training-examples across training-data do
	 (let ((this-state-gaussian (compute-vector-list-parameters training-examples))) ;; re-estimate the gaussian  <-- this is about 15% of the training time
	   (push this-state-gaussian state-gaussians)))
    (make-hmm :emission-distributions (coerce (nreverse state-gaussians) '(vector gaussian))
	      :transition-probabilities (estimate-transition-prob-matrix training-data example-count)
	      :state-count state-count
	      :word word
	      :state-ids state-ids)))

(defun estimate-transition-prob-matrix (training-data example-count)
  "Estimate the transition probabilities from training examples."
  (let* ((state-count (length training-data))
	 (matrix (make-array (list state-count (1+ state-count)) :initial-element 0.0 :element-type 'single-float))
	 (in-state-debug-info '()))
    (loop for this-segment-group across training-data
       for i from 0 to state-count do
	 (let* ((this-segment-group (elt training-data i))
		(in-state-count (length this-segment-group))
		(out-transition-count example-count)
		(transition-count (+ in-state-count out-transition-count)))
	   (push in-state-count in-state-debug-info)
	   (setf (aref matrix i i)  (float (/ in-state-count transition-count)))
	   (setf (aref matrix i (1+ i))  (float (/ out-transition-count transition-count)))))
    matrix))

