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

;;;; Trellis and trellis searching.  This is the core of the system in many ways.

(defmacro do-active-states ((var active-node-list) &body body)
  "Loop over the active states, setting the variable 'var' to state-id for each one."
  `(loop for active across ,active-node-list
      for ,var from 0 below (length ,active-node-list) do
	(when (= active 1)
	  ,@body)))

(defun print-rr (o s depth)
  "Print a recognition result object.  Doesn't print extremely verbose things, like the complete state-wise trellis back pointers."
  (declare (ignore depth))
  (format s "#S(CL-ASR::RECOGNITION-RESULT :WORDS ~A :RAW-WORDS ~A :SCORE ~f :RELATIVE-THRESHOLD ~A :BEAM-THRESHOLD ~A :NODES-VISITED-COUNT ~D :NODE-COUNT ~D :NODES-VISITED-COUNT-PERCENT ~F :TRELLIS-SEARCH-TIME ~A :TOTAL-TIME ~A :REAL-TIME-FACTOR ~F :WORD-PATH ~A :PHONEME-PATH ~A :STATE-PATH ~A)~%"
	  (rr-words o)
	  (rr-raw-words o)
	  (rr-score o)
	  (rr-relative-threshold o)
	  (rr-beam-threshold o)
	  (rr-nodes-visited-count o)
	  (rr-node-count o)
	  (/ (rr-nodes-visited-count o)
	     (rr-node-count o))
	  (rr-trellis-search-time o)
	  (rr-total-time o)
	  (rr-real-time-factor o)
	  (rr-word-path o)
	  (rr-phoneme-path o)
	  (rr-state-path o)))

(defstruct (recognition-result
	     (:conc-name rr-)
	     (:print-function print-rr))
  "The results and statistics of a trellis search."
  ;; The actual results
  words
  raw-words
  state-path-words
  score
  ;; Some of the parameters we used
  relative-threshold
  beam-threshold
  ;; Some stats on what happened
  nodes-visited-count
  node-count
  trellis-search-time
  total-time
  real-time-factor
  ;; Some path info
  word-path
  phoneme-path
  state-path
  ;; Complete data structures, if we saved them
  complete-trellis
  complete-bp-table
  trellis
  bp-list)

(defun print-bp (o stream depth)
  "Print a back pointer (in a way that's more readable than the default?)."
  (declare (ignore depth))
  (format stream "#S(CL-ASR::BACK-POINTER :WORD-ID \"~A\" :END-TIME ~d :BEGIN-TIME ~d :SCORE ~A :BP ~A )~%" (back-pointer-word-id o) (back-pointer-begin-time o) (back-pointer-end-time o) (back-pointer-cumulative-score o) (back-pointer-bp o)))

(defstruct (back-pointer
	     (:print-function print-bp))
  "Representation of a back-pointer for the back-pointer table?"
  word-id
  end-time
  begin-time
  bp
  cumulative-score
  total-score
  acoustic-score
  lm-score
  pos-tag-probs
  pos-tag-back-pointers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Illusory trellis..       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (trellis (:conc-name t-))
  "This struct holds all the data structures associated with the trellis search.
   This includes two columns of scores, back-pointer structures, etc."
  height
  width
  active-nodes                        ;; These are nodes that we have a "finger" on... i.e. there was some way to get to them thus far.
  next-active-nodes                   ;; These are the nodes that we will have a "finger" on in the next iteration ... i.e. everything we can get to from the places we're already at.
  hmm                                 ;; The language HMM
  scores                              ;; The scores for *this* column
  next-scores                         ;; The scores for the next column
  match-function
  frame-features                      ;; the actual feature values for the current frame (to match against HMMs)
  frame-number                        ;; previously known as the column number
  frame-back-pointer-vector           ;; ??
  previous-frame-back-pointer-vector  ;; ??
  back-pointer-table                  ;; ??
  use-lm                 
  complete-bp-table
  complete-trellis                    ;; are we using this...?
  insertion-penalty
  language-weight
  gaussian-match-cache   ;; we want to keep this, but the size of it is too big (should be the size of the # of senones)
  log-base)

(defun initialize-trellis (height width &key retain-complete-bp-table retain-complete-trellis hmm use-lm match-function insertion-penalty language-weight log-base)
  "Create an initial empty data structure."
  (let ((trellis (make-trellis :height height :width width :hmm hmm :use-lm use-lm :match-function match-function :insertion-penalty insertion-penalty :language-weight language-weight))
	(initial-back-pointer nil))
    ;; Initial score vectors
    (setf (t-scores trellis) (make-array height :initial-element nil :element-type '(or single-float null)))
    (setf (t-next-scores trellis) (make-array height :initial-element nil :element-type '(or single-float null)))
    ;; Initial (empty) active node vectors
    (setf (t-active-nodes trellis) (make-array height :initial-element 0 :element-type 'bit))
    (setf (t-next-active-nodes trellis) (make-array height :initial-element 0 :element-type 'bit))
    ;; Initial (and very high storage) optional book-keeping
    (when retain-complete-bp-table
      (setf (t-complete-bp-table trellis) (make-array (list width height) :element-type 'fixnum)))
    (when retain-complete-trellis
      (setf (t-complete-trellis trellis) (make-array (list width height) :element-type 'single-float)))
    ;; Local back pointers
    (setf (t-back-pointer-table trellis) (make-array 0 :element-type t :adjustable t :fill-pointer 0))
    (setf (t-frame-back-pointer-vector trellis) (make-array height :initial-element nil :element-type '(or null back-pointer)))  ;; default it to point to the start state
    (setf (t-previous-frame-back-pointer-vector trellis) (make-array height :initial-element initial-back-pointer :element-type '(or null back-pointer)))  ;; default it to point to the start state	 
    ;; A cache to store Gaussian match computations.
    (setf (t-gaussian-match-cache trellis) (make-array (length (acoustic-model-gmms *acoustic-model*)) :initial-element nil :element-type '(or null single-float)))
    (setf (t-log-base trellis) log-base)
    trellis))

(defun get-recognition-result (trellis hmm search-start-time retain-complete-bp-table height width relative-threshold beam-threshold nodes-visited)
  "Given the trellis and some aux info, follow the back-pointers, get the words, and create a recognition result object."
  (let* ( ;; Get the score, final state, and word hypothesis
	 (score (get-best-final-score (t-next-scores trellis) hmm))
	 (best-final-state (get-best-final-state (t-next-scores trellis) hmm))
	 (words (get-hypothesis-from-bp-table (t-previous-frame-back-pointer-vector trellis) best-final-state))
	 (bp-list (get-bp-list (aref (t-previous-frame-back-pointer-vector trellis) best-final-state)))
	 ;; Compute the total search time
	 (total-search-time (float (/ (- (get-internal-real-time) search-start-time) 1000)))
	 ;; If we saved the complete back-pointer table, get the state path, and the word hypothesis from that state path (for redundnacy and sanity check?).
	 (state-path (when retain-complete-bp-table (get-shortest-path-from-complete-bp-table (t-complete-bp-table trellis) (t-next-scores trellis) hmm height width)))
	 (state-path-words (get-hypothesis-from-path state-path hmm)))
    (make-recognition-result :score score ;; first the hypotheses and scores...
			     :words (remove-silences-and-sentence-boundaries words)
			     :raw-words words
			     :state-path-words state-path-words
			     ;; Some of the parameters we provided
			     :relative-threshold relative-threshold
			     :beam-threshold beam-threshold
			     ;; Some stats over the process
			     :trellis-search-time total-search-time
			     :node-count (* height width)
			     :nodes-visited-count nodes-visited
			     ;;:state-path state-path
			     :bp-list bp-list
			     ;; We need these to get the boundaries
			     :word-path (get-state-seq-word-seq state-path hmm)
			     :phoneme-path (get-state-seq-phoneme-seq state-path hmm)
			     ;; Complete data structures, if we saved them...
			     :complete-trellis (t-complete-trellis trellis)
			     :complete-bp-table (t-complete-bp-table trellis)
			     :trellis trellis)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Print debugging/progress ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-top-n-histories (trellis n &key in-progress)
  "Beginning from the top-scoring active state, and going to the lowest, find and return the top 'n' scoring
   distinct word histories.  Calling this function repeatedly will cause the decoding to slow down
   substantially."
  (let* ((history-table (make-hash-table :test 'equalp))
	 (states-table (make-hash-table :test 'equalp))
	 (state-score-pairs '())
	 (histories '()))
    ;; Get pairs of each score and each state
    (do-active-states (state-id (t-active-nodes trellis))
      (let ((score (aref (t-scores trellis) state-id)))
	(push (list state-id score) state-score-pairs)))
    ;; Sort them by score
    (setf state-score-pairs (sort state-score-pairs '> :key 'second))
    ;; Loop through all the state/score pairs
    (loop for (state score) in state-score-pairs
       for history = (get-bp-word-seq (aref (t-previous-frame-back-pointer-vector trellis) state)) do
	 (when in-progress
	   (setf history (nconc history (list (aref (language-hmm-hmm-state-words (t-hmm trellis)) state)))))
	 (unless (gethash history history-table)
	   (push (list history score) histories))
	 (incf (gethash history history-table 0))
	 (push state (gethash history states-table))
	 (when (>= (hash-table-count history-table) n)
	   (return)))
    (setf histories (mapcar (lambda (x) (append x (list (gethash (first x) history-table) (gethash (first x) states-table)   ))) histories))
    (nreverse histories)))

(defun print-top-n-histories (trellis n &key in-progress)
  "Call the function above to get the top-n histories, and print them readably."
  (declare (optimize (debug 3)))
  (let ((histories (get-top-n-histories trellis n :in-progress in-progress)))
    (loop for (history score count states) in histories
       for i from 1 to n do
	 (if in-progress
	     (format t "          ~:d. ~{~A ~} (~F) [count: ~:d] [states:~{ ~A~}]~%" i history score count (truncate-list states 8))
	     (format t "          ~:d. ~{~A ~} (~F) [count: ~:d]~%" i history score count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Helper/aux functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-ending-transition-p (hmm previous-state this-state)
  "Check if the transition from 'previous-state' to 'this-state' is 'word-ending'.
   That is, 'previous state' is that last state of a word, and 'this-state' is no longer the last
   state of a word (most likely it's a non-emitting state).  This function is especially important
   and frequently used because we often need to do two things at the end of a word:
    1) create a back pointer
    2) get a LM score."
  (declare (optimize (speed 3)))
  (and (svref (language-hmm-word-final-state hmm) previous-state)
       (not (svref (language-hmm-word-final-state hmm) this-state))))

(defun get-trellis-lm-score (state-id trellis)
  "Get a language model score for the word (ending?) at state 'state-id'.
   Function calls from this function end up consing, because they need to construct a list
   representing the word history."
  ;; Get a LM score...
  (let* ((hmm (t-hmm trellis))
	 (bp (aref (the (simple-array (or back-pointer null)) (t-previous-frame-back-pointer-vector trellis)) state-id))
	 (new-word (aref (the (simple-array (or string null)) (language-hmm-hmm-state-words hmm)) state-id))
	 (new-word-normalized (remove-alt-pron-marker new-word))
	 (lm-score (get-back-pointer-lm-score bp new-word-normalized)))
    (coerce lm-score 'single-float)))

(defun get-hmm-match-score (trellis emitting-state)
  "The gets the 'match score' for the current frame in the given HMM state.
   This function first checks if the score has already been computed and is cached.  If the score
   is not cached, it is computed and then cached for next time.  This cache is cleared out after
   each frame."
  (declare (optimize (speed 3)))
  (let* ((hmm (t-hmm trellis))
	 (distribution-id (svref (language-hmm-emission-distributions hmm) emitting-state)) ;; this should be null or an integer
	 (cached-match (when distribution-id (svref (t-gaussian-match-cache trellis) distribution-id)))
	 (distribution (svref (acoustic-model-gmms *acoustic-model*) distribution-id)))
    ;; If it's not yet cached, compute the match score and save it.
    (unless cached-match
      (setf (svref (t-gaussian-match-cache trellis) distribution-id) (funcall 'log-gaussian-mixture-probability (t-frame-features trellis) distribution)))
    ;; By now, we've computed the match score and it's in the cache, so just return what's in the cache.
    (svref (t-gaussian-match-cache trellis) distribution-id)))

(defun precompute-hmm-match-scores (trellis)
  (loop for distribution-id from 0 below (length (acoustic-model-gmms *acoustic-model*))
     for distribution = (svref (acoustic-model-gmms *acoustic-model*) distribution-id)
     for match = (funcall 'log-gaussian-mixture-probability (t-frame-features trellis) distribution) 
     for scaled-match = (/ match (the single-float (log (the single-float (t-log-base trellis))))) do
       (setf (svref (t-gaussian-match-cache trellis) distribution-id) match)
       (format t "ID: ~:D  SCORE: ~F SCALED: ~F~%" distribution-id match scaled-match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; The actual search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Most of the consing is probably happening in the places where we get the LM score.
(defun search-illusory-trellis (input-sequence hmm &key
				(relative-threshold nil) (beam-threshold nil) (max-width nil)
				(compare-function #'log-gaussian-mixture-probability)
				(retain-complete-bp-table t) (retain-complete-trellis t) (use-lm nil)
				insertion-penalty language-weight
				(debug t)
				(verbose t)
				(print-top-n 5)
				log-base)
  "The main trellis search function.  Unlike previous version of this function, the full
   trellis is never explicitly constructed (a huge efficiency gain), thus this function
   searches the 'illusory' trellis, not a 'real' one.  (This function may need to be re-written)."
  (declare ((simple-array simple-array) input-sequence))
  
  ;; Make sure all the arrays in the trellis are simple vectors
  (make-language-hmm-simple hmm)

  (let* ((search-start-time (get-internal-real-time))
	 (height (language-hmm-state-count hmm))
	 (width (length input-sequence))
	 (trellis (initialize-trellis height width :retain-complete-bp-table retain-complete-bp-table :retain-complete-trellis retain-complete-trellis :hmm hmm
				      :match-function compare-function :use-lm use-lm :insertion-penalty insertion-penalty :language-weight language-weight :log-base log-base))
	 (nodes-visited 0))
    (declare (fixnum width nodes-visited height))
    (format t "State count: ~:D~%" height)
    (setf (sbit (t-active-nodes trellis) (language-hmm-start-state hmm)) 1) ;; Make the start state active...
    (setf (svref (t-scores trellis) (language-hmm-start-state hmm)) 0.0)      ;; Start with a zero score
    ;; The main search loop
    (when max-width (setf width max-width))
    (loop for i from 0 below width
       for non-nil-scoring-state-count = (- (length (t-scores trellis)) (count nil (t-scores trellis)))
       for active-state-count = (count 1 (the simple-bit-vector (t-active-nodes trellis))) 
       for best-score = (get-best-score (t-active-nodes trellis) (t-scores trellis)) do
	 (when debug (format t "Working on column # ~5:D of ~5:D.  Active state count ~8:D.  Best score: ~,1f~%" (1+ i) width active-state-count best-score) (force-output))
	 ;; Do pruning here, first, so we don't prune the path to the final state on the last iteration/column
	 (cond ((and beam-threshold relative-threshold)
		(error "Can't perform beam threshold and relative threshold simultaneously!"))
	       (relative-threshold
		(prune-active-nodes (t-active-nodes trellis) (t-scores trellis) relative-threshold :verbose verbose :frame-no i))
	       (beam-threshold
		(prune-active-nodes-beam (t-active-nodes trellis) (t-scores trellis) beam-threshold :verbose verbose)))
	 (fresh-line t)	 
       ;; Put references to the frame number and the frame observation sequence in the trellis struct so we don't have to pass them around.
	 (setf (t-frame-features trellis) (aref input-sequence i))
	 (setf (t-frame-number trellis) i)
	 (compute-next-column trellis :verbose verbose)
	 ;; Note: this verbosity slows us down A LOT....
	 (when verbose
	   (format t "     ~:d BEST PATHS IN-PROGRESS:~%" print-top-n)
	   (print-top-n-histories trellis print-top-n :in-progress t)
	   (format t "     ~:d BEST HISTORIES:~%" print-top-n)
	   (print-top-n-histories trellis print-top-n :in-progress nil))
	 (incf nodes-visited (count 1 (the simple-bit-vector (t-next-active-nodes trellis))))
       ;; copy this column of the trellis back one
	 (replace (the simple-bit-vector (t-active-nodes trellis)) (the simple-bit-vector (t-next-active-nodes trellis)))
       ;; If we're saving the entire trellis, save that.
	 (when retain-complete-trellis
	   (dotimes (j height)
	     (setf (aref (the (simple-array single-float) (t-complete-trellis trellis)) i j) (svref (t-scores trellis) j)))))
    ;; end of the main viterbi loop
    (get-recognition-result trellis hmm search-start-time retain-complete-bp-table height width relative-threshold beam-threshold nodes-visited)))

;; In principle, we could add some threading to these next few functions...
(defun compute-next-column (trellis &key verbose)
  "Attempt to 'reach' everywhere in the next column we can get to from the current active states.
   This also does a lot of the before/after prepartion for moving to the next column: 1) clearing
   out the data structures that represent the next column, and 2) after copying the next column
   values to the new *this* column."
  (declare (optimize (speed 3)))
  ;; Empty the next column active list
  (fill (the simple-bit-vector (t-next-active-nodes trellis)) 0)
  ;; Clear out any cached gaussian computations
  (fill (the simple-array (t-gaussian-match-cache trellis)) nil)
  ;; reset the next column scores
  (fill (the simple-array (t-next-scores trellis)) nil)
  ;; Pre-compute all the gaussian-match scores (for debugging):
  ;;(precompute-hmm-match-scores trellis)
  ;; Then find all the places we can get to and add them to the next column active list...
  (do-active-states (state-id (the simple-bit-vector (t-active-nodes trellis)))
    (advance-by-one-state state-id trellis :verbose verbose))
  ;; Copy "next" column to this column, so we're ready for to iterate.
  (replace (the (simple-array (or null single-float)) (t-scores trellis))
	   (the (simple-array (or null single-float)) (t-next-scores trellis)))
  (replace (the (simple-array (or null back-pointer)) (t-previous-frame-back-pointer-vector trellis))
	   (the (simple-array (or null back-pointer)) (t-frame-back-pointer-vector trellis)))
  (fill (the simple-array (t-frame-back-pointer-vector trellis)) nil))

(defun advance-by-one-state (state-id trellis &key (debug nil) (verbose nil))
  "Call PUSH-TO-STATE on each of the successor states from state 'state-id'.  That function will call itself recursively
   (if necessary) until it gets to another *emitting* state."
  (declare (optimize (speed 3)))
  (let* ((hmm (t-hmm trellis))
	 (successors (aref (the (simple-array (cons fixnum)) (language-hmm-transitions hmm)) state-id))
	 (cumulative-score (the single-float (svref (t-scores trellis) state-id)))
	 (previous-back-pointer (svref (t-previous-frame-back-pointer-vector trellis) state-id)))
    (loop for successor of-type fixnum in successors
       for log-transition-probability of-type single-float in (aref (the (simple-array (cons single-float)) (language-hmm-transition-probabilities hmm)) state-id) do
	 (push-to-state state-id successor (+ log-transition-probability cumulative-score) trellis previous-back-pointer :debug debug :verbose verbose))
    (values)))

#+:pos-tagger
(defun initialize-initial-pos-bp (bp)
  (let ((s-1-tag (pos-tagger::get-tag-id :|<s-1>| pos-tagger::*pos-model*))
	(s-tag (pos-tagger::get-tag-id :|<s>| pos-tagger::*pos-model*)))
    (fill (back-pointer-pos-tag-back-pointers bp) s-1-tag)
    (fill (back-pointer-pos-tag-probs bp) sb-ext:single-float-negative-infinity)
    (setf (aref (back-pointer-pos-tag-probs bp) s-tag) 0.0)))
  
#+:pos-tagger
(defun follow-lattice-pos-back-pointers (final-bp final-state model)
  (let ((pos-list (list final-state))
	(word-list '())
	(prev final-state))
    (loop for bp = final-bp then (back-pointer-bp bp) 
       while bp do
	 (setf prev (aref (back-pointer-pos-tag-back-pointers bp) prev))
	 (push (back-pointer-word-id bp) word-list)
	 (push prev pos-list))
    (setf pos-list (mapcar (lambda (x) (pos-tagger::get-tag x model)) pos-list))
    (format t "WORD LIST:~{ ~A~}~%" word-list)
    (format t "POS LIST: ~{ ~A~}~%" pos-list)
    pos-list))

#+:pos-tagger
(defun pos-viterbi-for-new-bp (back-pointer);; previous-back-pointer);; word-ending)
  (when pos-tagger::*pos-model*
    (let ((word-ending (back-pointer-word-id back-pointer))
	  (previous-back-pointer (back-pointer-bp back-pointer))
	  (pos-scores (make-array (pos-tagger::pos-model-tag-count pos-tagger::*pos-model*) :initial-element 0.0 :element-type 'single-float))
	  (pos-back-pointers (make-array (pos-tagger::pos-model-tag-count pos-tagger::*pos-model*) :initial-element nil :element-type '(or null fixnum))))
      (setf (back-pointer-pos-tag-probs back-pointer) pos-scores)
      (setf (back-pointer-pos-tag-back-pointers back-pointer) pos-back-pointers)
      ;; Don't do viterbi just for "<s>"...
      (cond ((string-equal word-ending "<s>")
	     (initialize-initial-pos-bp back-pointer))
	    ((string-equal word-ending "</s>")
	     (follow-lattice-pos-back-pointers back-pointer (pos-tagger::get-tag-id :|</s>| pos-tagger::*pos-model*) pos-tagger::*pos-model*))
	    ;; If it's a silence... just copy the POS vectors through...
	    ((string-equal word-ending "<sil>")
	     (setf (back-pointer-pos-tag-probs back-pointer) (back-pointer-pos-tag-probs previous-back-pointer))
	     (setf (back-pointer-pos-tag-back-pointers back-pointer) (back-pointer-pos-tag-back-pointers previous-back-pointer)))
	    ;; Otherwise, we do the normal viterbi thing...
	    (t
	     (pos-tagger::viterbi-one-step word-ending pos-scores pos-back-pointers 
					   (back-pointer-pos-tag-probs previous-back-pointer)
					   (back-pointer-pos-tag-back-pointers previous-back-pointer)))
	    ))))

(defun update-cumulative-score-with-lm (begin-state trellis cumulative-score)
  "Get a LM score for the ending word, adjust the log base, scale by the language weight,
   add to the cumulative score along with the insertion penalty."
  ;; Increment the score by the LM score...
  (let ((lm-score (get-trellis-lm-score begin-state trellis)))
    (declare (single-float lm-score))
    ;; Convert the LM score to the appropriate log base
    (setf lm-score (/ lm-score (the single-float (log (the single-float (t-log-base trellis)) (coerce (language-model::lm-log-base *lm*) 'single-float)))))
    ;; Multiply the LM score by the language weight
    (setf lm-score (* lm-score (the single-float (t-language-weight trellis))))
    ;; Add the scaled LM score to the current cumulative score and the insertion penalty since we just added a word.
    (values (+ cumulative-score lm-score (the single-float (t-insertion-penalty trellis))) lm-score)))

(defun get-adjusted-lm-score (lm-score trellis)
  "Get a LM score for the ending word, adjust the log base, scale by the language weight,
   add to the cumulative score along with the insertion penalty."
  ;; Increment the score by the LM score...
  (declare (single-float lm-score))
  ;; Convert the LM score to the appropriate log base
  (setf lm-score (/ lm-score (the single-float (log (the single-float (t-log-base trellis)) (coerce (language-model::lm-log-base *lm*) 'single-float)))))
  ;; Multiply the LM score by the language weight
  (setf lm-score (* lm-score (the single-float (t-language-weight trellis))))
  ;; Add the scaled LM score to the current cumulative score and the insertion penalty since we just added a word.
  (+ lm-score (the single-float (t-insertion-penalty trellis))))

(defun get-new-bp (trellis begin-state end-state cumulative-score previous-back-pointer)
  (let* ((previous-cumulative-score (if previous-back-pointer (back-pointer-cumulative-score previous-back-pointer) 0.0))
	 (new-cumulative-score cumulative-score)
	 (lm-score 0.0)
	 (hmm (t-hmm trellis))
	 (word-ending (aref (the (simple-array (or null string)) (language-hmm-hmm-state-words hmm)) begin-state)))    
    (assert (word-ending-transition-p hmm begin-state end-state))
    ;; If we're using the LM, and the word we're ending isn't a silence... then we need an LM score.
    (when (and (t-use-lm trellis) (string-not-equal word-ending "<sil>"))
      (let* ((this-lm-score (get-trellis-lm-score begin-state trellis))
	     (adjusted-lm-score (get-adjusted-lm-score this-lm-score trellis)))
	(setf new-cumulative-score (+ cumulative-score adjusted-lm-score))
	(setf lm-score this-lm-score)))    
    (let* ((back-pointer (make-back-pointer :word-id word-ending
					    :end-time (t-frame-number trellis)
					    :begin-time (if previous-back-pointer (back-pointer-end-time previous-back-pointer) 0)
					    :bp previous-back-pointer
					    :cumulative-score cumulative-score
					    :total-score (- cumulative-score (the single-float previous-cumulative-score))
					    :lm-score lm-score
					    :acoustic-score (- cumulative-score (the single-float previous-cumulative-score)))))
      back-pointer)))

;; ~73% OF THE TIME IS IN THE FUNCTION (WHEN DOING A FLAT DECODE)
(defun push-to-state (begin-state end-state cumulative-score trellis previous-back-pointer &key (debug nil) (verbose nil));; recursion-depth)
  "Advance from active state 'begin-state' to 'end-state', accumulating all the various components of the score as we go.  If end-state
   is not an emitting state, then call this function recursively on all of end-state's successors until we reach an emitting state."
  (declare (optimize (speed 3))
	   (single-float cumulative-score))
  (let* ((hmm (t-hmm trellis))
	 (emitting-state end-state) ;; seems like this could go either way...
	 (hmm-state-distribution (aref (the (simple-array (or null fixnum)) (language-hmm-emission-distributions hmm)) emitting-state))
	 (non-emitting-state-p (not hmm-state-distribution)))

    (when (word-ending-transition-p hmm begin-state end-state)
      (setf previous-back-pointer (get-new-bp trellis begin-state end-state cumulative-score previous-back-pointer))
      #+:pos-tagger
      (when pos-tagger::*pos-model*
	(pos-viterbi-for-new-bp previous-back-pointer)))

    (if non-emitting-state-p
	;; if it's non-emitting, then we have to recurse til we get to an omitting state
	(progn
	  (loop for next-state of-type fixnum in (elt (the (simple-array (or cons null)) (language-hmm-transitions hmm)) end-state)
	     for next-log-transition-probability of-type single-float in (aref (the (simple-array (or (cons single-float) null)) (language-hmm-transition-probabilities hmm)) end-state) do
	       (push-to-state end-state
			      next-state
			      (+ next-log-transition-probability cumulative-score) trellis previous-back-pointer :debug debug :verbose verbose))
	  ;; If there are no out-going states... then we're at the *trellis* end state
	  (when (and (not (elt (the (simple-array (or cons null)) (language-hmm-transitions hmm)) end-state))
		     (or (not (aref (the (simple-array (or null single-float)) (t-next-scores trellis)) end-state))
			 (> cumulative-score (aref (the (simple-array (or null single-float)) (t-next-scores trellis)) end-state))))
	    (assert (= end-state 3))
	    (setf (aref (the (simple-array (or null back-pointer)) (t-frame-back-pointer-vector trellis)) end-state) previous-back-pointer)
	    (setf (aref (the (simple-array (or null single-float)) (t-next-scores trellis)) end-state) cumulative-score)))
	;; If it is emitting...
	(progn
	  (let* ((match-score (get-hmm-match-score trellis emitting-state)))
	    (declare (single-float match-score))
	    (setf match-score (/ match-score (the single-float (log (the (single-float (0.0) *) (t-log-base trellis))))))
	    (setf cumulative-score (+ cumulative-score match-score))
	    ;; Add this successor state to the next column's active "list"
	    (setf (sbit (t-next-active-nodes trellis) end-state) 1)
	    (when (or (not (svref (t-next-scores trellis) end-state))
		      (> cumulative-score (the single-float (svref (t-next-scores trellis) end-state))))
	      (setf (svref (t-frame-back-pointer-vector trellis) end-state) previous-back-pointer)
	      (setf (svref (t-next-scores trellis) end-state) cumulative-score)   
	      (vector-push previous-back-pointer (t-back-pointer-table trellis))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Hypothesis retrieval functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-hypothesis-from-bp-table (frame-back-pointer-vector end-state-num)
  "Get the best word seqeunce from the given back-pointer table (actually a vector)."
  (let ((bp (aref frame-back-pointer-vector end-state-num)))
    (format t "END STATE NUMBER: ~A~%" end-state-num)
    (format t "Last back-pointer: ~A~%" bp)
    (get-bp-word-seq bp)))

(defun get-bp-word-seq (bp &key exclude-silences)
  "Given a back pointer, follow the pointers and collect the words along the way,
   until we have the complete word sequence."
  (declare (optimize (speed 3)))
  (let ((word-list '()))
    (loop until (not bp) do
	 (when (or (not exclude-silences) (string/= (back-pointer-word-id bp) "<sil>"))
	   (push (back-pointer-word-id bp) word-list))
	 (setf bp (back-pointer-bp bp)))
    word-list))

(defun get-bp-list (bp &key exclude-silences)
  "Given a back pointer, follow the pointers and collect the words along the way,
   until we have the complete bp sequence."
  (let ((bp-list '()))
    (loop until (not bp) do
	 (when (or (not exclude-silences) (string/= (back-pointer-word-id bp) "<sil>"))
	   (push bp bp-list))
	 (setf bp (back-pointer-bp bp)))
    bp-list))

(defun remove-silences (word-list)
  "Given a list of words, remove all instances of '<sil>'.  Used the clean-up the final hypothesis."
  (setf word-list (remove "<sil>" word-list :test 'string-equal))
  word-list)

(defun remove-silences-and-sentence-boundaries (word-list)
  "Given a list of words, remove all instances of '<sil>', '<s>', and '</s>'.
   Used the clean-up the final hypothesis."
  (setf word-list (remove-silences word-list))
  (setf word-list (remove "<s>" word-list :test 'string-equal))
  (setf word-list (remove "</s>" word-list :test 'string-equal))
  word-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; LM Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-back-pointer-lm-score (bp word)
  "Given a back-pointer, get the word history, and then return the LM prob of the history."
  (declare (optimize (speed 3)))
  (let ((word-history (get-bp-word-seq bp :exclude-silences t)))
    (language-model::log-prob-of-history *lm* (nconc word-history (list word)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Pruning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-best-score (active-node-list this-column)
  (let ((best most-negative-single-float))
    ;; First find the best-scoring node
    (do-active-states (e active-node-list)
      (let ((this-score (aref this-column e)))
	(when (> this-score best)
	  (setf best this-score))))
    best))

(defun get-relative-threshold (active-node-list this-column delta)
  "Given the active nodes, a score vector, and a delta distance from the 'best' score:
   Find the best scoring active node, subtract the delta from it, and return the difference
   which can be used as a threshold."
  (let ((best most-negative-single-float))
    ;; First find the best-scoring node
    (do-active-states (e active-node-list)
      (let ((this-score (aref this-column e)))
	(when (> this-score best)
	  (setf best this-score))))
    (- best (abs delta))))

;;; WE CAN DO THIS FASTER!!  I.E. WITHOUT THE "SORT"
;;; OR, WE CAN JUST USE THE RELATIVE THRESHOLD ANYWAY, SINCE THAT'S EASILY LINEAR (FINDING THE MIN/MAX)
;;; SO, UNTIL WE GET THE RELATIVE THRESHOLD WORKING PROPERLY/BETTER, THIS IS WORTH IT...
(defun get-beam-threshold (active-node-list this-column beam-width)
  "Given the active nodes, a score vector, and a suggested beam width:
   Find the threshold that will ensure a beam that is *at least* as wide as the suggested width."
  (let ((scores '()))
    (do-active-states (e active-node-list)
      (let ((this-score (aref this-column e)))
	(push this-score scores)))
    (setf scores (sort scores '>))
    (setf beam-width (min beam-width (1- (length scores))))
    (if (>= beam-width 0)
	(elt scores beam-width)
	most-negative-single-float)))

(defun prune-with-threshold (active-node-list this-column threshold &key (verbose t))
  "Given the active node list, prune those which are below the given *absolute* threshold."
  (let ((pruned-nodes-count 0)
	(best most-negative-single-float)
	(highest-prunable-node-id (- (length active-node-list) 3))
	(active-node-count-after-pruning 0))
    (do-active-states (e active-node-list)
      (let ((this-score (aref this-column e)))
	(when (and verbose (> this-score best))
	  (setf best this-score))
	;; Not sure if this is right....  don't prune the states leading up to the final state?
	(when (and (< this-score threshold)
		   (< e highest-prunable-node-id)
		   (> e 9)
		   (/= e 3))
	  (setf (aref active-node-list e) 0)
	  (incf pruned-nodes-count))))
    (setf active-node-count-after-pruning (count 1 active-node-list))
    (when verbose (format t "  Nodes pruned: ~10:D (Scores; Best: ~8,3f; Thresh: ~8,3f; Diff: ~,3f). Remaining active nodes: ~8:D.~%" pruned-nodes-count best threshold (- best threshold) active-node-count-after-pruning))
    (values pruned-nodes-count best threshold (- best threshold))))

(defun prune-active-nodes (active-node-list this-column relative-threshold &key (verbose t) (frame-no nil))
  "Given the active node list, prune those which are beyond the threshold from the best.
   To avoid any consing, or complexity, rather than truly pruning the list, inactive nodes are
   set to the value most-positive-fixnum."
  ;; Maybe something like this will help?!?
  (setf relative-threshold (* relative-threshold (1+ frame-no)))
  (let ((threshold (get-relative-threshold active-node-list this-column relative-threshold)))
    (prune-with-threshold active-node-list this-column threshold :verbose verbose)))

(defun prune-active-nodes-beam (active-node-list this-column width &key (verbose t))
  "Given the active node list, prune those which are beyond the threshold from the best.
   To avoid any consing, or complexity, rather than truly pruning the list, inactive nodes are
   set to the value most-positive-fixnum."
  (let ((threshold (get-beam-threshold active-node-list this-column width)))
    (prune-with-threshold active-node-list this-column threshold :verbose verbose)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Methods for getting relevant representations of the shortest path through the trellis. ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-hypothesis-from-path (path hmm)
  "Get the word hypothesis from the 'path.'  Here the 'path' is a sequence of states.
   This function depends on the fact that non-emitting state are in the path (I believe)."
  (setf path (map 'list (lambda (x) (elt (language-hmm-hmm-state-words hmm) x)) path)) ;; get the corresponding words
  (let ((hypothesis '())
	(previous nil))
    (dolist (e path)
      (unless (equal e previous) ;; only save it if we've reached a new word
	(push e hypothesis))
      (setf previous e))
    ;; Remove any potential silence markers and null values
    (setf hypothesis (delete "silence" hypothesis :test 'string-equal))
    (setf hypothesis (delete "sil" hypothesis :test 'string-equal))
    (setf hypothesis (delete "<sil>" hypothesis :test 'string-equal))
    (setf hypothesis (delete nil hypothesis))
    (nreverse hypothesis)))

(defun get-state-seq-word-seq (path hmm)
  "Get a list of the words that we were in at each moment in time through the best
   path.  Non-emitting states will show up as null.  This will likely break when we
   fix the non-emitting states problem."
  (map 'list (lambda (x) (elt (language-hmm-hmm-state-words hmm) x)) path))

(defun get-state-seq-phoneme-seq (path hmm)
  "Get a list of the phonemes that we were in at each moment in time through the best
   path.  Non-emitting states will show up as null.  This will likely break when we
   fix the non-emitting states problem."
  (map 'list (lambda (x) (elt (language-hmm-hmm-state-phonemes hmm) x)) path))

(defun get-shortest-path-from-complete-bp-table (bp-table last-column hmm height width)
  "Given the complete back pointers table, find the best final state, and then find the
   path back from that state to the start state.  The shortest path is represented simply
   as a list of state ids."
  (let* ((best-final-state (get-best-final-state last-column hmm))
	 (path (trace-path-from-final-state bp-table hmm height width best-final-state)))
    path))

(defun trace-path-from-final-state (bp-table hmm height width final-state)
  "Given a final state and the complete back-pointer table, trace the shortest path back
   to the start state."
  (declare (ignore hmm height))
  (let ((path (list final-state))
	(current-state final-state))
    (loop for i from (1- width) downto 0 do
	 (let ((previous-state (aref bp-table i current-state)))  ;;this gets the previous state b/c it's looking at a bp matrix
	   (push previous-state path)
	   (setf current-state previous-state)))
    path))

(defun get-best-final-state (last-column hmm)
  "Given the last column of the trellis search, and the language HMM, find the
   'final' state with the best score.  There may be more than one final state."
  (let ((final-states (language-hmm-final-states hmm))
	(best-final-state nil)
	(best-final-state-score nil))
    (dolist (final-state final-states)
      (let ((score-to-this-final-state (aref last-column final-state)))
	(when (or (not best-final-state-score)
		  (and score-to-this-final-state (> score-to-this-final-state best-final-state-score)))
	  (setf best-final-state-score score-to-this-final-state)
	  (setf best-final-state final-state))))
    (values best-final-state best-final-state-score)))

(defun get-best-final-score (last-column hmm)
  "Given the last column of the trellis search, and the language HMM, find the
   'final' state with the best score.  Return the score at that best final state."
  (multiple-value-bind (best-state best-score)
      (get-best-final-state last-column hmm)
    (declare (ignore best-state))
    best-score))

(defun print-path (path hmm)
  "Print a shortest in a slightly more readable way.  Show the word at every single frame;
   it's not condensed down into the words themselves."
  (dolist (state path)
    (format t "~A " (elt (language-hmm-hmm-state-words hmm) state)))
  (write-line ""))
