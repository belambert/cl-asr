;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "New (and improved!) version of function to add triphones to a language HMM")

(defun create-template-language-hmm (acoustic-model log-base)
  "This function creates a basic template language HMM that can be augmented to create a real, useful language HMM."
  (declare (optimize (debug 3)))
  (let ((hmm (create-initial-language-hmm-new log-base :initial-size 50))
	(sil-phone (get-matching-triphones acoustic-model :base "SIL")))
    (assert (= (length sil-phone) 1))
    (setf sil-phone (first sil-phone))
    ;; Add leading and trailing silences aka begin/end sentence markers
    (add-triphone-to-hmm :hmm hmm :triphone sil-phone :source-state 0 :dest-state 1 :word "<s>" :word-final t :log-base log-base)
    (add-triphone-to-hmm :hmm hmm :triphone sil-phone :source-state 2 :dest-state 3 :word "</s>" :word-final t :log-base log-base)
    hmm))

(defun add-triphone-to-hmm (&key hmm triphone word word-final log-base source-state dest-state entry-table exit-table word-id)
  "Add the given triphone to the given HMM. This function is really important!
   All the functions that construct language HMMs call this function numerous times!"
  (unless log-base
    (setf log-base (language-hmm-log-base hmm)))

  (let* ((first-state (language-hmm-state-count hmm))
	 (triphone-tmat (get-triphone-tmat triphone))
	 (triphone-state-count (length (triphone-model-state-ids triphone)))
	 ;; always add an extra non-emitting state at the end.
	 (states-to-add (1+ triphone-state-count))
	 (last-state (1- (+ first-state states-to-add))))
    ;; Extend the HMM
    (extend-language-hmm-by-n-states hmm states-to-add)                           ;; Make the language HMM bigger

    ;; Keep track of what word/triphone we're putting here..
    (fill (language-hmm-hmm-state-phonemes hmm) triphone :start first-state :end (1+ last-state))   ;; Save which triphone this is...
    (fill (language-hmm-hmm-state-words hmm) word :start first-state :end (1+ last-state))          ;; Save which word this is...

    (fill (language-hmm-word-id hmm) word-id :start first-state :end (1+ last-state))               ;; Save the word id
    (fill (language-hmm-phone-id hmm) (language-hmm-triphone-count hmm) :start first-state :end (1+ last-state))             ;; Save the phone id

    (setf (gethash (language-hmm-triphone-count hmm) (language-hmm-phone-id-table hmm)) triphone)

    ;; Increment the number of triphones in the model (also used as the triphone ID)
    (incf (language-hmm-triphone-count hmm))

    ;; Copy over the emission distributions
    (replace (language-hmm-emission-distributions hmm) (get-triphone-emissions triphone) :start1 first-state)
    ;; Next we need the transitions and transitions probabilities...
    (loop for x from 0 below (array-dimension triphone-tmat 0) do
	 (loop for y from 0 below (array-dimension triphone-tmat 1) do
	      (let ((trans-prob (aref triphone-tmat x y)))
		(when (and trans-prob (/= trans-prob 0.0))
		  (if (/= y (1- (array-dimension triphone-tmat 1)))
		      (connect-lang-hmm-states (+ first-state x) (+ first-state y) hmm (log trans-prob log-base))
		      (connect-lang-hmm-states (+ first-state x) last-state hmm (log trans-prob log-base)))))))
    (when word-final (setf (aref (language-hmm-word-final-state hmm) last-state) t))
    (assert (= (+ states-to-add first-state) (language-hmm-state-count hmm)))
    ;; Save the states to the word entry/exit tables if they were given
    (when entry-table
      (push first-state (gethash (list (triphone-model-left triphone) (triphone-model-base triphone)) entry-table)))
    (when exit-table
      (push last-state (gethash (list (triphone-model-base triphone) (triphone-model-right triphone)) exit-table)))
    ;; Make the source and destination state connections if those states are given.
    (when source-state (connect-lang-hmm-states source-state first-state hmm 0.0))
    (when dest-state (connect-lang-hmm-states last-state dest-state hmm 0.0))
    (values first-state last-state)))

(defun add-triphone-seq-to-hmm (&key hmm triphone-seq source-state dest-state word word-final log-base word-id)
  "Convenience function... calls the function above, but on a sequence of triphones, chaining them together in series."
  ;;(assert dest-state) (assert source-state)
  (let ((initial-state nil)
	(final-state nil)
	(prev-last-state nil)
	(triphone-count (length triphone-seq)))
  (loop for triphone in triphone-seq
     for i from 0 below triphone-count do       
       (multiple-value-bind (first-state last-state)
	   (if (= i (1- triphone-count))
	       (add-triphone-to-hmm :hmm hmm :triphone triphone :source-state prev-last-state :word word :word-final word-final :log-base log-base :word-id word-id)
	       (add-triphone-to-hmm :hmm hmm :triphone triphone :source-state prev-last-state :word word :log-base log-base :word-id word-id))
	 (when (= i 0) (setf initial-state first-state))
	 (when (= i (1- triphone-count)) (setf final-state last-state))
	 (when prev-last-state (connect-lang-hmm-states prev-last-state first-state hmm 0.0))
	 (setf prev-last-state last-state)))
  ;; If source and/or destiniation states are given, make the connections
  (when source-state (connect-lang-hmm-states source-state initial-state hmm 0.0))
  (when dest-state (connect-lang-hmm-states final-state dest-state hmm 0.0))
  (values initial-state final-state)))

(defun add-parallel-triphones-to-hmm (&key hmm triphones source-state dest-state word word-final log-base entry-table exit-table word-id)
  "Convenience function... calls the function above.  This adds any number of triphones to the HMM, connected so that they are 'in parallel'.
   Apparently, this function doesn't actually do anything to connect the triphones... rather it seems to be relying on the caller to specify
   shared source and/or destination states that cause the triphones to exist in parallel."
  (loop for triphone in triphones do
       (add-triphone-to-hmm :hmm hmm :triphone triphone :source-state source-state :dest-state dest-state :word word :word-final word-final :log-base log-base :entry-table entry-table :exit-table exit-table  :word-id word-id))
  (values source-state dest-state))


