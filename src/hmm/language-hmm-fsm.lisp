;;;; Author: Ben Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "The original language HMM construction code -- for constructing a language HMM from a word FSM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This part if for alignment/training... ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "This part if for alignment/training...")
					      
(defvar *silence-id-string* "<sil>"
  "String that designates silence (e.g. 'SIL', '<SIL>', 'SILENCE').  Alternatively,
   we might want this to be a set of strings.")

(defun create-fsm-from-word-seq (word-seq &key (mandatory-begin-and-end-silence t) (insert-optional-silences-between-words t) debug)
  "Create a finite state machine struct from a word sequence...  this is used
   to build a language HMM from a training sentence."
  (when mandatory-begin-and-end-silence
    (when (not (stringp mandatory-begin-and-end-silence))
      (setf mandatory-begin-and-end-silence *silence-id-string*))
    (setf word-seq (append (list mandatory-begin-and-end-silence) word-seq (list mandatory-begin-and-end-silence))))

  (when debug (format t "Creating FSM for word seq: ~{~A~^ ~}~&" word-seq))
  (let ((state-count (1+ (length word-seq)))
	(start-state 0)
	(terminal-states (list (length word-seq)))
	(node-list '())
	(edge-list '())
	(vocab (remove-duplicates (append word-seq (list *silence-id-string*)) :test 'string-equal)))
    ;; Create all the nodes up front
    (dotimes (i state-count)
      (push (make-fsm-node :id i :lang-hmm-id i) node-list))
    ;; Connect the nodes with the words
    (loop for word in word-seq
       for i from 0 to (length word-seq) do
	 (push (make-fsm-edge :source-id i   ;; connect node i to node i+1 with the current word
			      :destination-id (1+ i)
			      :label (elt word-seq i))
	       edge-list))
    ;; Add self-transitions, labeled with silence, to model the optional silences
    (when insert-optional-silences-between-words
      (loop for i from 0 to state-count do
	   (push (make-fsm-edge :source-id i  ;; self transition
				:destination-id i
				:label *silence-id-string*)
		 edge-list)))
    ;; Now we've got everything, create the struct we need
    (make-fsm :state-count state-count
	      :start-state start-state
	      :terminal-states terminal-states
	      :edge-list (nreverse edge-list)
	      :node-list (nreverse node-list)
	      :edge-count (length edge-list)
	      :vocab vocab)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Composing word HMMs from phone HMMs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Composing word HMMs from phone HMMs")

(defun compose-word-hmm-from-phoneme-models-ci (phone-table word)
  "Compose a *regular* HMM (as opposed to a language HMM) from CONTEXT INDEPENDENT phone models.
   This literally concatenate the phoneme models."
  (let* ((phonemes (get-phonemes-for-word word)) ;; all the phonemes for this word...
	 (word-emission-dists '())
	 (transition-matrices '())
	 (state-ids '())
	 (phoneme-state-ids '()))
    (unless phonemes (format t "Warning: no dictionary phonemes found for word ~A.  Dictionary not loaded?~%" word))    
    ;; First collect up all the info we need
    (loop for i from 0 below (length phonemes)
       for phoneme = (elt phonemes i) do
	 (let* ((this-phone-hmm (gethash phoneme phone-table))
		(this-phone-emission-dists (hmm-emission-distributions this-phone-hmm)))
	   (setf word-emission-dists (concatenate 'vector word-emission-dists this-phone-emission-dists))
	   (push (hmm-transition-probabilities this-phone-hmm) transition-matrices)
	   ;; this keeps track of which states belong to which phonemes
	   (dotimes (i (hmm-state-count this-phone-hmm))
	     (push (hmm-word this-phone-hmm) state-ids)
	     (push (list (hmm-word this-phone-hmm) i) phoneme-state-ids))))    
    ;; Make sure it's in the right order
    (setf transition-matrices (nreverse transition-matrices))
    (setf state-ids (nreverse state-ids))
    (setf phoneme-state-ids (nreverse phoneme-state-ids))
    ;; Construct a new HMM with all these pieces
    (let* ((emissions (make-array (length word-emission-dists) :element-type '(or gaussian null) :initial-contents word-emission-dists))
	   (state-count (length emissions))
	   (new-hmm (make-instance 'hmm :word word
				   :state-count state-count
				   :emission-distributions emissions
				   :transition-probabilities (compose-transition-matrices transition-matrices)
				   :state-ids state-ids
				   :phoneme-state-ids phoneme-state-ids)))
      (assert (= state-count (* (length phonemes) 3)))
      new-hmm)))

(defun compose-word-hmm-from-phoneme-models (phone-table word)
  "Compose a *regular* HMM (as opposed to a language HMM) from CONTEXT DEPENDENT phone models.
   This literally concatenate the phoneme models."
  (let* ((phonemes (get-phonemes-for-word word)) ;; all the phonemes for this word...
	 (word-emission-dists '())
	 (transition-matrices '())
	 (state-ids '())
	 (phoneme-state-ids '())
	 (phoneme-count (length phonemes))
	 (previous-phone nil))
    (unless phonemes (format t "Warning: no dictionary phonemes found for word ~A.  Dictionary not loaded?~%" word))    
    ;; First collect up all the info we need
    (loop for i from 0 below (length phonemes)
       for phoneme = (elt phonemes i) do
	 (let* ((key (cond ((= i 0) ;; it's the first phone of the word...
			    phoneme)
			   ((= i (1- phoneme-count))  ;; it's the last phone...
			    phoneme)
			   (t
			    (list phoneme previous-phone (elt phonemes (1+ i)) :internal))))
		(this-phone-hmm (if (gethash key phone-table)
				    (gethash key phone-table)
				    (gethash phoneme phone-table)))
		(this-phone-emission-dists (hmm-emission-distributions this-phone-hmm)))
	   (setf previous-phone phoneme)
	   (setf word-emission-dists (concatenate 'vector word-emission-dists this-phone-emission-dists))
	   (push (hmm-transition-probabilities this-phone-hmm) transition-matrices)
	   ;; this keeps track of which states belong to which phonemes
	   (dotimes (i (hmm-state-count this-phone-hmm))
	     (push (hmm-word this-phone-hmm) state-ids)
	     (push (list (hmm-word this-phone-hmm) i) phoneme-state-ids))))    
    ;; Make sure it's in the right order
    (setf transition-matrices (nreverse transition-matrices))
    (setf state-ids (nreverse state-ids))
    (setf phoneme-state-ids (nreverse phoneme-state-ids))
    ;; Construct a new HMM with all these pieces
    (let* ((emissions (make-array (length word-emission-dists) :element-type '(or gaussian null) :initial-contents word-emission-dists))
	   (state-count (length emissions))
	   (new-hmm (make-instance 'hmm :word word
				   :state-count state-count
				   :emission-distributions emissions
				   :transition-probabilities (compose-transition-matrices transition-matrices)
				   :state-ids state-ids
				   :phoneme-state-ids phoneme-state-ids)))
      (assert (= state-count (* (length phonemes) 3)))
      new-hmm)))

(defun compose-transition-matrices (matrix-list)
  "Given a list of transition matrices... concatenate them with one another."
  (unless matrix-list
    (return-from compose-transition-matrices nil))
  (let* ((state-count (array-dimension (first matrix-list) 0))
	 (main-dimension (* state-count (length matrix-list)))
	 (new-matrix (make-array (list main-dimension (1+ main-dimension)) :element-type 'single-float :initial-element 0.0))
	 (x-offset 0)
	 (y-offset 0))
    (dolist (matrix matrix-list)
      (loop for x from 0 below (array-dimension matrix 0) do
	   (loop for y from 0 below (array-dimension matrix 1) do
		(assert (= (aref new-matrix (+ x x-offset) (+ y y-offset)) 0.0))
		(setf (aref new-matrix (+ x x-offset) (+ y y-offset))
		      (aref matrix x y))))
      (incf x-offset state-count)
      (incf y-offset state-count))
    new-matrix))

(defun get-word-hmm-tables (fsm phone-hmm-table &key ci)
  "Create a table of word HMM models for all the words in the given FSM, using the phones in the given phone-hmm-table.
   Optionally, only use context independent phone models (for debugging)."
  ;; Create some tables and get the vocab
  (let ((word-hmm-state-count-table (make-hash-table :test 'equalp))
	(word-hmm-table (make-hash-table :test 'equalp)) ;; This contains word HMM's...
	(vocab (fsm-vocab fsm)))
    ;; Compose word HMM's for each word in the vocabulary
    (dolist (word vocab)
      (unless (or (string-equal word "<s>") (string-equal word "</s>"))
	(let ((word-hmm (if ci
			    (compose-word-hmm-from-phoneme-models-ci phone-hmm-table word)
			    (compose-word-hmm-from-phoneme-models phone-hmm-table word))))
	  (setf (gethash word word-hmm-state-count-table) (hmm-state-count word-hmm))
	  (setf (gethash word word-hmm-table) word-hmm))))
    (values word-hmm-table word-hmm-state-count-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Construction of language HMMs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Construction of language HMMs")


(defun number-hmm-states (fsm word-hmm-state-count-table)
  "Uniquely number all the states in the language HMM.  The FSM stores the state IDs."
  (let ((lang-hmm-state-counter (fsm-state-count fsm))) ;; start the counter at the number of states in the FSM, so the non-emitting states are all up front...
    (dolist (edge (fsm-edge-list fsm))
      (when (fsm-edge-label edge)
	(let* ((word (fsm-edge-label edge))
	       (word-hmm-state-count (gethash word word-hmm-state-count-table)))
	  (unless (or (string-equal word "<s>") (string-equal word "</s>"))
	    (setf (fsm-edge-lang-hmm-start-id edge) lang-hmm-state-counter)
	    ;; Is "end" non-inclusive...?  It must be...
	    (setf (fsm-edge-lang-hmm-end-id edge) (+ lang-hmm-state-counter (1- word-hmm-state-count)))
	    (incf lang-hmm-state-counter word-hmm-state-count)))))
    lang-hmm-state-counter))

(defun add-word-edge-to-language-hmm (lang-hmm edge word-hmm-table &key silence-penalty insertion-penalty)
  "Add a single word edge to the given, partially constructed language HMM."
  (let* (;; First get the relevant info from the edge
	 (source-node (fsm-edge-source-id edge))
	 (dest-node (fsm-edge-destination-id edge))
	 (label (fsm-edge-label edge))
	 (start (fsm-edge-lang-hmm-start-id edge))
	 (end (fsm-edge-lang-hmm-end-id edge))
	 ;; Then get the relevant info from the language hmm
	 (hmm-state-words (language-hmm-hmm-state-words lang-hmm))
	 (hmm-state-phonemes (language-hmm-hmm-state-phonemes lang-hmm))
	 (word-final-state (language-hmm-word-final-state lang-hmm))
	 (emission-distribution-table (language-hmm-emission-distributions lang-hmm))
	 ;; this is where we get the word's HMM...
	 (word-hmm (gethash label word-hmm-table)))
    (if (not label)
	;; no penalty for edges with no label
	(connect-lang-hmm-states source-node dest-node lang-hmm 0.0)  ;; if there's no label, connect the source and destination directly, with no cost/score for the edge
	(progn ;;o/w
	  ;; connect the non-emitting states to the beginning and end of the hmm
	  (if (or (string-equal label "SIL") (string-equal label "<SIL>"));;(string-equal label "<S>")(string-equal label "</s>")
	      (connect-lang-hmm-states source-node start lang-hmm silence-penalty)
	      (connect-lang-hmm-states source-node start lang-hmm insertion-penalty))
	  (connect-lang-hmm-states end dest-node lang-hmm 0.0) ;; no penalty for leaving a word
	  ;; For each state in the edge's word's HMM:
	  (loop for i from start to end do   ;; only allow staying in the same state or moving directly to the next
	       (let ((word-hmm-state-id (- i start)))       ;; which state of the word's HMM are we in?
		 ;; save the word and the phoneme for where we're at!
		 (setf (elt hmm-state-words i) label)
		 (setf (elt hmm-state-phonemes i) (elt (hmm-state-ids word-hmm) word-hmm-state-id))
		 ;; Save the particular word and state number for the word, and the phoneme...
		 (setf (elt (language-hmm-word-state-map lang-hmm) i) (list label word-hmm-state-id))
		 ;; Save the number of the phoneme's state if we have it.
		 (when (hmm-phoneme-state-ids word-hmm) (setf (elt (language-hmm-phoneme-state-map lang-hmm) i) (elt (hmm-phoneme-state-ids word-hmm) word-hmm-state-id)))
		 ;; save the emission dist.
		 (setf (elt emission-distribution-table i) (elt (hmm-emission-distributions word-hmm) word-hmm-state-id))
		 ;; Make a connection for the self transition
		 (connect-lang-hmm-states i i lang-hmm  (log (aref (hmm-transition-probabilities word-hmm) word-hmm-state-id word-hmm-state-id))) ;;; <-- the last term is the edge's score
		 ;; Make a transition to the next state, if we're not at the end of the word
		 (if (= i end)
		     (setf (aref word-final-state i) t)  ;; Keep track of which are the word final states; the back pointer table needs them
		     (connect-lang-hmm-states i (1+ i) lang-hmm (log (aref (hmm-transition-probabilities word-hmm) word-hmm-state-id (1+ word-hmm-state-id)))))
		 ))))))
  
(defun create-language-hmm-from-fsm (fsm phone-hmm-table &key (model-source :phone) silence-penalty insertion-penalty ci)
  "Given an FSM specification and a folder containing word HMMs, create a language HMM.
   This is a big, gnarly function."
  (declare (optimize (debug 3)))
  (unless silence-penalty (setf silence-penalty 0.0))
  (unless insertion-penalty (setf insertion-penalty 0.0))
  ;; Compose the phone HMMs into word HMMs
  (multiple-value-bind (word-hmm-table word-hmm-state-count-table)
      (get-word-hmm-tables fsm phone-hmm-table :ci ci)
    ;; Figure out how many states will be in the language HMM (not too hard since it's flat)
    (let ((hmm-state-count (number-hmm-states fsm word-hmm-state-count-table)))
      (assert (equal model-source :phone))
      ;; Now create the transition table
      (let* ((lang-hmm (create-initial-language-hmm fsm hmm-state-count)) ;; create the initial empty language HMM
	     (transition-table (language-hmm-transitions lang-hmm))
	     (transition-probability-table (language-hmm-transition-probabilities lang-hmm)))
	;; Now add the components of the word HMMs to the empty language HMM
	(dolist (edge (fsm-edge-list fsm))
	  (add-word-edge-to-language-hmm lang-hmm edge word-hmm-table :silence-penalty silence-penalty :insertion-penalty insertion-penalty))
	;; We're not actually reversing these arrays, we're reversing the *contents* of each array cell (since we pushed onto them as we proceeded).
	(map-into transition-table 'nreverse transition-table)
	(map-into transition-probability-table 'nreverse transition-probability-table)
	(setf (language-hmm-state-count lang-hmm) (length transition-table))
	lang-hmm))))

(defun create-fsm-and-language-hmm-from-fsm (vocab acoustic-model &key silence-penalty insertion-penalty ci phoneme-recognition align-reference)
  "Create a FSM flat bigram grammar from the given vocab, then build a language HMM out of that."
  (let* ((fsm (cond (align-reference
		     (create-fsm-from-word-seq align-reference :mandatory-begin-and-end-silence nil :insert-optional-silences-between-words nil))
		    ((equal phoneme-recognition :ci)
		     ;;(build-flat-phoneme-fsm :ci-only t)
		     (error "Phoneme recognition disabled in this version"))
		    (phoneme-recognition
		     ;;(build-flat-phoneme-fsm)
		     (error "Phoneme recognition disabled in this version"))
		    (t (build-flat-bigram-fsm vocab))))
	 (lang-hmm (create-language-hmm-from-fsm fsm acoustic-model
						 :model-source :phone
						 :silence-penalty silence-penalty
						 :insertion-penalty insertion-penalty
						 :ci ci)))
    lang-hmm))

	
