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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Language HMM representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct language-hmm
  "A language HMM.  This is constructed by replacing the edges in an FSM with word HMMs."
  start-state
  final-states
  (transitions #() ) ;;:type (simple-array (or (cons fixnum) null)))
  (transition-probabilities #() ) ;;:type (simple-array (or (cons (or single-float null)) null)))
  (emission-distributions #() ) ;;:type (simple-array (or null fixnum)))
  (hmm-state-words #() ) ;;:type (simple-array (or null string))) ;; where the words are...
  (hmm-state-phonemes #() );; :type (simple-array (or null string))) ;; where the phonemes are...
  state-count
  (edge-count 0)
  ;; This tells you if you're in the 1st state of the word HMM, the second state of the word HMM, etc.
  word-state-map       ;; holds the word id and the word-hmm state id
  ;; This tell you which state of the phone HMM you're in...
  phoneme-state-map    ;; hold the phoneme id and the phoneme-hmm state id
  ;; Tells you if this state represents the end of a word.
  (word-final-state #() ) ;;:type (simple-array boolean))
  state-names
  word-id
  phone-id
  log-base
  (triphone-count 0)
  (phone-id-table (make-hash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Construction of language HMMs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-state-word-final (hmm state)
  (setf (aref (language-hmm-word-final-state hmm) state) t))

(defun set-state-word (hmm state word)
  (setf (aref (language-hmm-hmm-state-words hmm) state) word))

(defun print-lang-hmm-dead-ends (hmm)
  (let ((count 0))
    (loop for i from 0 below (language-hmm-state-count hmm) do
	 (unless (aref (language-hmm-transitions hmm) i)
	   (incf count)
	   (format t "No transitions from state ~10:D (~A ~A)~%" i
		   (aref (language-hmm-hmm-state-words hmm) (1- i))
		   (aref (language-hmm-hmm-state-words hmm) i))))
    (format t "~:D language HMM dead ends found.~%" count)))

(defun connect-lang-hmm-states (source dest hmm edge-value &key verbose)
  "Perform the necessary actions to connect two nodes in a language HMM.
   A value, or cost, can also be assigned to the edge."
  (when verbose (format t "Creating edge: ~:D --> ~:D~%" source dest))
  (let* ((transition-table (language-hmm-transitions hmm))
	 (transition-probabilities (language-hmm-transition-probabilities hmm)))
    (incf (language-hmm-edge-count hmm))
    (push dest (elt transition-table source))
    (push edge-value (elt transition-probabilities source))))

(defun create-initial-language-hmm (fsm state-count)
  "Create an initial, initialized language HMM for the given FSM, and state-count.
   It's tricky to figure out how many states the language HMM should have, so it must be
   specified by the caller."
  (make-language-hmm :transitions (make-array state-count :initial-element nil)
		     :transition-probabilities (make-array state-count :initial-element nil)
		     :emission-distributions (make-array state-count :initial-element nil)
		     :start-state (fsm-start-state fsm)
		     :final-states (fsm-terminal-states fsm)
		     :hmm-state-words (make-array state-count :initial-element nil)
		     :hmm-state-phonemes (make-array state-count :initial-element nil)
		     :word-state-map (make-array state-count :initial-element nil)    ;; training is using this one-- word-state-map
		     :phoneme-state-map (make-array state-count :initial-element nil) ;; training is using this one-- word-state-map
		     :word-final-state (make-array state-count :initial-element nil)))

(defun make-language-hmm-simple (hmm)
  (setf (language-hmm-transitions hmm) (make-array (language-hmm-state-count hmm) :initial-contents (language-hmm-transitions hmm) :element-type '(or (cons fixnum) null)))
  (setf (language-hmm-transition-probabilities hmm) (make-array (language-hmm-state-count hmm) :initial-contents (language-hmm-transition-probabilities hmm) :element-type '(or (cons (or single-float null)) null)))
  (setf (language-hmm-emission-distributions hmm) (make-array (language-hmm-state-count hmm) :initial-contents (language-hmm-emission-distributions hmm) :element-type '(or gaussian null)))
  (setf (language-hmm-hmm-state-words hmm) (make-array (language-hmm-state-count hmm) :initial-contents (language-hmm-hmm-state-words hmm) :element-type '(or null string)))
  (setf (language-hmm-hmm-state-phonemes hmm) (make-array (language-hmm-state-count hmm) :initial-contents (language-hmm-hmm-state-phonemes hmm) :element-type '(or null string)))
  (setf (language-hmm-word-final-state hmm) (make-array (language-hmm-state-count hmm) :initial-contents (language-hmm-word-final-state hmm) :element-type 'boolean))
  (map-into (language-hmm-transitions hmm) 'nreverse (language-hmm-transitions hmm))
  (map-into (language-hmm-transition-probabilities hmm) 'nreverse (language-hmm-transition-probabilities hmm))
  hmm)

(defun create-initial-language-hmm-new (log-base &key (initial-states 4) (initial-size 0))
  (when (< initial-size initial-states)
    (setf initial-size initial-states))
  (let ((hmm (make-language-hmm :transitions (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:transition-probabilities (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:emission-distributions (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:start-state 0
				:final-states (list 3)
				:state-count initial-states
				:hmm-state-words (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:hmm-state-phonemes (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:word-final-state (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:state-names (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:word-id (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:phone-id (make-array initial-size :initial-element nil :fill-pointer initial-states :adjustable t)
				:log-base log-base
				)))
    hmm))

(defun extend-language-hmm-by-n-states (hmm n)
  (dotimes (i n)
    (vector-push-extend nil (language-hmm-transitions hmm))
    (vector-push-extend nil (language-hmm-transition-probabilities hmm))
    (vector-push-extend nil (language-hmm-emission-distributions hmm))
    (vector-push-extend nil (language-hmm-hmm-state-words hmm))
    (vector-push-extend nil (language-hmm-hmm-state-phonemes hmm))
    (vector-push-extend nil (language-hmm-word-final-state hmm))
    (vector-push-extend nil (language-hmm-state-names hmm))
    (vector-push-extend nil (language-hmm-word-id hmm))
    (vector-push-extend nil (language-hmm-phone-id hmm)))
  (incf (language-hmm-state-count hmm) n))

(defun get-new-language-hmm-state (hmm &key name)
  "Unlike the previous one, this returns the ID of the new state..."
  (let ((new-state (1- (extend-language-hmm-by-n-states hmm 1))))
    (when name
      (setf (aref (language-hmm-state-names hmm) new-state) name))
    new-state))

(defun list->numbered-string (list)
  (with-output-to-string (s)
    (loop for e in list
       for i from 0 below (length list) do
	 (format s "~:D: ~A. " i e))))

(defun list-list->numbered-string (list)
  (with-output-to-string (s)
    (loop for e in list
       for i from 0 below (length list) do
	 (format s "~:D: ~{~A~^, ~}. " i e))))

(defun print-language-hmm-summary (hmm &key (stream t))
  (let ((non-emitting-state-count (count nil (language-hmm-emission-distributions hmm)))
	(emitting-state-count (count-if 'identity (language-hmm-emission-distributions hmm))))
    (format stream "#<LANGUAGE HMM: states: ~:D, emitting-states: ~:D, non-emitting-states: ~:D, edges: ~:D,  triphone-count: ~:D>~%"
	    (language-hmm-state-count hmm)
	    emitting-state-count
	    non-emitting-state-count
	    (language-hmm-edge-count hmm)	  
	    (language-hmm-triphone-count hmm))))

(defun make-triphone-readable (triphone)
  (if (triphone-p triphone)
      (if (or (triphone-left triphone) (triphone-right triphone))
	  (format nil "~A_~A_~A" (triphone-base triphone) (string-downcase (triphone-left triphone)) (string-downcase (triphone-right triphone)))
	  (format nil "~A" (triphone-base triphone)))
      nil))

(defun print-language-hmm (hmm &key max-count)
  (format t "LANGUAGE HMM. STATE=~A~%" (language-hmm-state-count hmm))
  (format t "TRANS:     ~A~%" (list-list->numbered-string (truncate-list (coerce (language-hmm-transitions hmm) 'list) max-count)))
  (format t "TRAN PROB: ~A~%" (list-list->numbered-string (truncate-list (coerce (language-hmm-transition-probabilities hmm) 'list) max-count)))
  (format t "WORDS:     ~A~%" (list->numbered-string (truncate-list (coerce (language-hmm-hmm-state-words hmm) 'list) max-count)))
  (format t "PHONES:    ~A~%" (list->numbered-string (truncate-list (mapcar 'make-triphone-readable (coerce (language-hmm-hmm-state-phonemes hmm) 'list)) max-count)))
  (format t "FINALP:    ~A~%" (list->numbered-string (truncate-list (coerce (language-hmm-word-final-state hmm) 'list) max-count))))

(defun triphone-acoustic-equalp (t1 t2)
  "Checks if two triphone *acoustic models* are really the same."
  (and (= (triphone-model-tmat t1) (triphone-model-tmat t2))
       (equalp (triphone-model-state-ids t1) (triphone-model-state-ids t2))))

(defun get-triphone-emissions (triphone)
  "Get the (id's)of the triphone's emission distributions."
  (triphone-model-state-ids triphone))

(defun get-triphone-tmat (triphone)
  (aref (acoustic-model-tmats sphinx-l::*acoustic-model*) (triphone-model-tmat triphone)))

(defun get-matching-triphones (acoustic-model &key base left right position unique singleton)
  (unless acoustic-model
    (setf acoustic-model *acoustic-model*))
  (let ((matches (gethash (list base left right position) (acoustic-model-triphone-table acoustic-model))))
    (unless matches
      (setf matches (get-ci-triphone-list base)))
    (setf matches (reverse matches))
    (when unique
      (setf matches (remove-duplicates matches :test 'sphinx-l::triphone-acoustic-equalp)))
    (when singleton
      (assert (= (length matches) 1))
      (setf matches (first matches)))
    matches))

(defun get-ci-triphone-list (base)
  (let ((matches (gethash (list base nil nil nil) (acoustic-model-triphone-table *acoustic-model*))))
    (assert (= (length matches) 1))
    matches))

(defun get-ci-triphone (base)
  (let ((matches (gethash (list base nil nil nil) (acoustic-model-triphone-table *acoustic-model*))))
    (assert (= (length matches) 1))
    (first matches)))

(defun get-word-triphone-seq (word &key left right singleton left-ci right-ci)
  (let ((triphones '())
	(phone-seq (get-phonemes-for-word word)))
    (coercef phone-seq 'vector)
    (if (= (length phone-seq) 1)
	(push (get-matching-triphones nil :base (aref phone-seq 0) :left left :right right :position :single-word :unique t) triphones)
	(loop for phone across phone-seq
	   for i from 0 below (length phone-seq) do
	     (let ((triphone (cond
			       ;; This is the first phone
			       ((= i 0)
				(if left-ci
				    (get-ci-triphone-list phone)
				    (get-matching-triphones nil :base phone :left left :right (aref phone-seq (+ i 1)) :position :begin)))
			       ;; This is the last phone
			       ((= i (1- (length phone-seq)))
				(if right-ci
				    (get-ci-triphone-list phone)
				    (get-matching-triphones nil :base phone :left (aref phone-seq (- i 1)) :right right :position :end)))
			       ;; These are all the middle phones
			       (t (let ((middle-triphones (get-matching-triphones nil :base phone :left (aref phone-seq (- i 1)) :right (aref phone-seq (+ i 1)) :position :internal)));; :unique t)))
				    (assert (= (length middle-triphones) 1))
				    middle-triphones)))))
	       (push triphone triphones))))
    (setf triphones (reverse triphones))
    (when singleton
      (assert (every (lambda (x) (= (length x) 1)) triphones))
      (setf triphones (mapcar 'first triphones)))
    triphones))

(defun get-word-triphone-list-seq (word)
  (let ((triphones '())
	(phone-seq (get-phonemes-for-word word)))
    (if (= (length phone-seq) 1)
	(push (list (first phone-seq) nil nil :single-word) triphones)
	(loop for phone in phone-seq
	   for i from 0 below (length phone-seq) do
	     (let ((triphone (cond ((= i 0)
				    (list phone nil (elt phone-seq (+ i 1)) :begin))
				    ((= i (1- (length phone-seq)))
				     (list phone (elt phone-seq (- i 1)) nil :end))
				    (t
				     (list phone (elt phone-seq (- i 1)) (elt phone-seq (+ i 1)) :internal)))))
	       (push triphone triphones))))
    (reverse triphones)))
