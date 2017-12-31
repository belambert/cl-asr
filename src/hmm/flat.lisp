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

(in-package :cl-asr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Adding flat words to the language HMM ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-flat-ci-word-to-hmm (word hmm)
  "Add a word, flat, with only CI phones to the hmm."
  (let* ((triphones (get-word-triphone-seq word :left-ci t :right-ci t :singleton t)))
    (add-triphone-seq-to-hmm :hmm hmm :triphone-seq triphones :source-state 1 :dest-state 2 :word word :word-final t)))

(defun add-flat-word-to-hmm (word hmm)
  "A previous version of the function below, to add a flat word, with internal CD triphones to the HMM."
  (let* ((triphone-seq (get-word-triphone-seq word))
	 (initial-triphones (first triphone-seq))
	 (rest-triphones (rest triphone-seq))
	 (dummy (assert (every (lambda (x) (= (length x) 1)) rest-triphones)))
	 (rest-triphones (mapcar 'first rest-triphones))
	 (rest-begin-state (add-triphone-seq-to-hmm :hmm hmm :triphone-seq rest-triphones :word word :word-final t :dest-state 2 :word word)))
    (declare (ignore dummy)
	     (fixnum rest-begin-state))
    (dolist (initial-triphone initial-triphones)
      (if (= (length triphone-seq) 1)
	  (add-triphone-to-hmm :hmm hmm :triphone initial-triphone :source-state 1 :dest-state 2 :word word :word-final t)
	  (add-triphone-to-hmm :hmm hmm :triphone initial-triphone :source-state 1 :dest-state rest-begin-state :word word :word-final nil)))))

(defun add-flat-cd-word-to-hmm-basic (word hmm &key left-ci right-ci)
  "Add a word with CD phones in the middle, optionally CD or CI phones on the ends, but connect
   the beginning and end states directly to states #1 and #2."
  (let* ((triphone-seq (get-word-triphone-seq word :left-ci left-ci :right-ci right-ci))
	 (phone-count (length triphone-seq))
	 (word-start (get-new-language-hmm-state hmm))
	 (word-end (get-new-language-hmm-state hmm)))
    ;; Connect the word begin/end to the rest of the HMM
    (connect-lang-hmm-states 1 word-start hmm 0.0)
    (connect-lang-hmm-states word-end 2 hmm 0.0)
    (let ((prev-state word-start))
      (loop for phone-position in triphone-seq
	 for i from 0 below phone-count
	 for dest-state = (if (= i (1- phone-count))
			      word-end
			      (if (> (length phone-position) 1)
				  (get-new-language-hmm-state hmm)
				  nil)) do
	   (if (> (length phone-position) 1)
	       (add-parallel-triphones-to-hmm :hmm hmm :triphones phone-position :source-state prev-state :dest-state dest-state :word word :word-id word)
	       (multiple-value-bind (start end)
		   (add-triphone-to-hmm :hmm hmm :triphone (first phone-position) :source-state prev-state :dest-state dest-state :word word :word-id word)
		 (declare (ignore start))
		 (setf dest-state end)))
	   (setf prev-state dest-state)))
    (setf (aref (language-hmm-word-id hmm) word-start) word)
    (setf (aref (language-hmm-word-id hmm) word-end) word)
    (set-state-word-final hmm word-end)
    (set-state-word hmm word-end word)
    (values word-start word-end)))

(defun add-flat-cd-word-to-hmm (word hmm &key left-ci right-ci entry-table exit-table)
  "Add a word with CD phones in the middle, optionally CD or CI phones on the ends.  Don't connect the triphones
   at the beginning and end of the word to anything, but keep track of all the begin/end states in the word
   entry/exit tables."
  (let* ((triphone-seq (get-word-triphone-seq word :left-ci left-ci :right-ci right-ci))
	 (phone-count (length triphone-seq)))
    (let ((prev-state nil)
	  (dest-state nil))
      (loop for phone-position in triphone-seq
	 for i from 0 below phone-count do	   
	   ;; Create a non-emitting destination state, *for the first phone only*
	   (when (and (= i 0) (not dest-state))
	    (setf dest-state (get-new-language-hmm-state hmm)))
	   (when dest-state
	     (setf (aref (language-hmm-word-id hmm) dest-state) word))
	   ;; This asserts that we only have one version of each middle phone...
	   (when (and (/= i 0) (/= i (1- phone-count)))
	     (assert (= (length phone-position) 1)))
	   (if (> (length phone-position) 1)
	       (cond (;; The first and only
		      (and (= i 0) (= i (1- phone-count)))
		      (add-parallel-triphones-to-hmm :hmm hmm :triphones phone-position :source-state prev-state :dest-state dest-state :entry-table entry-table :exit-table exit-table :word word :word-final t :word-id word))
		     (;; The first
		      (= i 0)
		      (add-parallel-triphones-to-hmm :hmm hmm :triphones phone-position :source-state prev-state :dest-state dest-state :entry-table entry-table :word word :word-id word))
		     (;; The last
		      (= i (1- phone-count))
		      (add-parallel-triphones-to-hmm :hmm hmm :triphones phone-position :source-state prev-state :dest-state dest-state :exit-table exit-table :word word :word-final t :word-id word))
		     (;; In the middle
		      t
		      (add-parallel-triphones-to-hmm :hmm hmm :triphones phone-position :source-state prev-state :dest-state dest-state :word word :word-id word)))
	       ;; we should be ending up in here in all the middle phones...?
	       (multiple-value-bind (start end)
		   ;;(add-triphone-to-hmm :hmm hmm :triphone (first phone-position) :source-state prev-state :dest-state dest-state)
		   (cond (;; The first and only
			  (and (= i 0) (= i (1- phone-count)))
			  (add-triphone-to-hmm :hmm hmm :triphone (first phone-position) :source-state prev-state :dest-state dest-state :entry-table entry-table :exit-table exit-table :word word :word-final t :word-id word))
			 (;; The first
			  (= i 0)
			  (add-triphone-to-hmm :hmm hmm :triphone (first phone-position) :source-state prev-state :dest-state dest-state :entry-table entry-table :word word :word-id word))
			 (;; The last
			  (= i (1- phone-count))
			  (add-triphone-to-hmm :hmm hmm :triphone (first phone-position) :source-state prev-state :dest-state dest-state :exit-table exit-table :word word :word-final t :word-id word))
			 (;; In the middle
			  t
			  (add-triphone-to-hmm :hmm hmm :triphone (first phone-position) :source-state prev-state :dest-state dest-state :word word :word-id word)))
		 (declare (ignore start))		 
		 ;; Save this destination state as the source state for the next phone
		 (setf dest-state end)))
	   (setf prev-state dest-state)
	   (setf dest-state nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions for creating the loop-back links we need for true CD models ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-table-duplicates (table)
  "Remove duplicates from the loop back table."
  (loop for key being the hash-keys of table
     for value = (gethash key table) do
       (setf (gethash key table) (remove-duplicates-fast value :ht-test 'eql))))

(defun get-prefix-agnostic-phone-keys (table)
  "Get a list of all the phone keys that are agnostic to the left context."
  (loop for key being the hash-keys of table
     for (p1 p2) being the hash-keys of table
     unless p1 collecting key))

(defun get-postfix-agnostic-phone-keys (table)
  "Get a list of all the phone keys that are agnostic to the right context."
  (loop for key being the hash-keys of table
     for (p1 p2) being the hash-keys of table
     unless p2 collecting key))

(defun make-loop-back-connnections-full (hmm entry-table exit-table &key verbose)
  "This makes a lot of connections for a flat structure.  For a 20k vocab, without compressing any
   equivalent states, it creates about 400 million connections.  So, it's one connection between each pair of words
   (even if only the beginning or end of the word is context-dependent).  Thus, we create |vocab|^2 connections here.
   Does this buy us anything?  Probably not, since we bypass through the non-emitting states during the search anyway."
  (format t "WARNING: Creating full set of loop-back connections!~%")
  (remove-table-duplicates entry-table)
  (remove-table-duplicates exit-table)
  (when verbose
    (format t "PREFIX AGNOSTIC WORD ENTRY PHONE KEYS:~{ ~A~}~%" (mapcar 'second (get-prefix-agnostic-phone-keys entry-table)))
    (format t "POSTFIX AGNOSTIC WORD EXIT PHONE KEYS:~{ ~A~}~%" (mapcar 'first (get-postfix-agnostic-phone-keys exit-table))))
  ;; Connect all the pairs of true triphones (directly)
  (let ((merged-keys (union (hash-table-keys entry-table) (hash-table-keys exit-table)))
	(loop-back-connection-count 0))
    (loop for key in merged-keys
       for (prev-phone next-phone) in merged-keys
       for exit-states = (append (gethash key exit-table) (gethash (list prev-phone nil) exit-table))
       for entry-states = (append (gethash key entry-table) (gethash (list nil next-phone) entry-table)) do
	 (dolist (i exit-states)
	   (dolist (j entry-states)
	     (incf loop-back-connection-count)
	     (when (= (mod loop-back-connection-count 1000) 0)
	       (format t "Created ~:D loop back connections...~%" loop-back-connection-count))
	     (connect-lang-hmm-states i j hmm 0.0 :verbose nil)))))

  ;; Connect the context agnostic phones
  (loop for first in (get-postfix-agnostic-phone-keys exit-table) 
     for exit-states = (gethash first exit-table) do
       (loop for second in (get-prefix-agnostic-phone-keys entry-table)
	    for entry-states = (gethash second entry-table) do
	    (dolist (i exit-states)
	      (dolist (j entry-states)
		(connect-lang-hmm-states i j hmm 0.0 :verbose nil))))))

(defun make-loop-back-connnections-basic (hmm entry-table exit-table &key verbose)
  "This creates loop-back links to a single 'collector' node for each biphone transition.
   This should create about 2 x |vocab| edges in the HMM graph."
  (remove-table-duplicates entry-table)
  (remove-table-duplicates exit-table)
  (when verbose
    (format t "PREFIX AGNOSTIC WORD ENTRY PHONE KEYS:~{ ~A~}~%" (mapcar 'second (get-prefix-agnostic-phone-keys entry-table)))
    (format t "POSTFIX AGNOSTIC WORD EXIT PHONE KEYS:~{ ~A~}~%" (mapcar 'first (get-postfix-agnostic-phone-keys exit-table))))
  ;; Connect all the pairs of true triphones (through a non-emitting collector node)
  (let ((merged-keys (union (hash-table-keys entry-table) (hash-table-keys exit-table))))
    (loop for key in merged-keys
       for (prev-phone next-phone) in merged-keys
       for exit-states = (append (gethash key exit-table) (gethash (list prev-phone nil) exit-table))
       for entry-states = (append (gethash key entry-table) (gethash (list nil next-phone) entry-table)) do
	 (when (and entry-states exit-states)
	   (let ((intermediate-state (get-new-language-hmm-state hmm :name (format nil "Loop-back state for biphone: ~A,~A" prev-phone next-phone))))
	     (setf (aref (language-hmm-word-id hmm) intermediate-state) (format nil "Loop-back state for biphone: ~A,~A" prev-phone next-phone))
	     (dolist (i exit-states)
	       (connect-lang-hmm-states i intermediate-state hmm 0.0))
	     (dolist (i entry-states)
	       (connect-lang-hmm-states intermediate-state i hmm 0.0))))))
  ;; Connect the context agnostic phones
  (loop for first in (get-postfix-agnostic-phone-keys exit-table) 
     for exit-states = (gethash first exit-table) do
       (loop for second in (get-prefix-agnostic-phone-keys entry-table)
	    for entry-states = (gethash second entry-table) do
	    (dolist (i exit-states)
	      (dolist (j entry-states)
		(connect-lang-hmm-states i j hmm 0.0 :verbose nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Other important connections for CD models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-initial-connections (hmm table)
  "Create edges from state #1 to the word initial phones that begin with a triphone that wants
   a left context of SIL."
  (loop for key being the hash-keys of table
     for (left right) = key
     for states = (gethash key table) do
       (when (or (not left) (string-equal left "SIL"))
	 (dolist (state states)
	   (connect-lang-hmm-states 1 state hmm 0.0)))))

(defun make-final-connections (hmm table &key (verbose t))
  "Create edges to state #2 from the word final phones that end with a triphone that wants
   a right context of SIL."
  (when verbose (format t "Making connections from end of each word to state #2.~%"))
  (loop for key being the hash-keys of table
     for (left right) = key
     for states = (gethash key table) do
       (when (or (not right) (string-equal right "SIL"))
	 (dolist (state states)
	   (connect-lang-hmm-states state 2 hmm 0.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High level functions that return language HMMs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-flat-ci-language-hmm (vocab acoustic-model log-base)
  "Create a language HMM comprised of flat words that are entirely composed of CI phone models."
  (let ((hmm (create-template-language-hmm acoustic-model log-base)))
    ;; Allow a loop back to state #1
    (connect-lang-hmm-states 2 1 hmm 0.0)
    ;; Add each word...
    (pushnew "<sil>" vocab :test 'string-equal)
    (dolist (word vocab)
      (add-flat-ci-word-to-hmm word hmm))
    (make-language-hmm-simple hmm)
    hmm))

(defun create-flat-cd-language-hmm-basic (vocab acoustic-model log-base &key left-ci right-ci) 
  "Create a language HMM comprised of flat words composed of CD phone models.
   This language HMM is 'basic' in terms of the way it makes the loop back connections.
   That is, there's a single loop-back link that entirely disregards the acoustic context
   before and after."
  (let ((hmm (create-template-language-hmm acoustic-model log-base)))
    ;; Allow a loop back to state #1
    (connect-lang-hmm-states 2 1 hmm 0.0)
    ;; Add each word...
    (pushnew "<sil>" vocab :test 'string-equal)
    (dolist (word vocab)
      (add-flat-cd-word-to-hmm-basic word hmm :left-ci left-ci :right-ci right-ci))
    (make-language-hmm-simple hmm)
    hmm))

(defun create-flat-cd-language-hmm (vocab acoustic-model log-base &key left-ci right-ci (loop-back :basic))
  "Create a language HMM comprised of flat words composed of CD phone models.
   This language HMM only has acoustically compatible loop-back edges."
  (let ((hmm (create-template-language-hmm acoustic-model log-base))
	(entry-table (make-hash-table :test 'equalp))
	(exit-table (make-hash-table :test 'equalp)))
    ;; Add each word...
    (pushnew "<sil>" vocab :test 'string-equal)
    (loop for word in vocab
       for i from 0 below (length vocab) do
	 (when (= (mod i 1000) 0)
	   (format t "Added ~:D vocab words.~%" i))
	 (add-flat-cd-word-to-hmm word hmm :left-ci left-ci :right-ci right-ci :entry-table entry-table :exit-table exit-table))
    ;; Connect states 1 and 2 to the middle states...
    (make-initial-connections hmm entry-table)
    (make-final-connections hmm exit-table)
    ;; Make the loop-back connections...
    (case loop-back
      (:basic (make-loop-back-connnections-basic hmm entry-table exit-table))
      (:full (make-loop-back-connnections-full hmm entry-table exit-table))
      (otherwise (error "Lookback method is required.")))
    (make-language-hmm-simple hmm)
    hmm))
