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

(defun build-align-language-hmm (words acoustic-model log-base &key (allow-alt-pron t))
  "Build an alignment language HMM.  This is the main function to call."
  (let ((hmm (create-template-language-hmm acoustic-model log-base)))
    (if allow-alt-pron
	(build-align-hmm-with-alt-pron words hmm :log-base log-base)
	(build-align-hmm words hmm :log-base log-base))
    (make-language-hmm-simple hmm)
    hmm))

(defun build-align-hmm (words hmm &key log-base (add-optional-silences t))
  "Build an alignment HMM, but used the exact words as they are given, and don't include alternate pronunciations.
   This is the kind of thing you'd want to do for a 'forced' alignment(?)."
  (setf words (coerce words 'vector))
  (let ((prev-last-state 1) ;; previous last state is state #1 (after the opening silence).
	(sil-phone (get-matching-triphones *acoustic-model* :base "SIL" :singleton t )))
    (loop for word across words do
      (unless (get-phonemes-for-word word) (error "Can't align.  Word '~A' not in dictionary!" word)))
    (when add-optional-silences (add-triphone-to-hmm :hmm hmm :triphone sil-phone :word "<sil>" :word-final t :log-base log-base :source-state prev-last-state :dest-state prev-last-state))
    (loop for i from 0 below (length words)
       for word = (elt words i)
       for prev-word = (if (/= i 0) (elt words (1- i)) "<s>")
       for next-word = (if (/= i (1- (length words))) (elt words (1+ i)) "</s>")
       for prev-phone = (last-elt (get-phonemes-for-word prev-word))
       for next-phone = (first (get-phonemes-for-word next-word))
       for triphones = 	(get-word-triphone-seq word :left prev-phone :right next-phone :singleton t) do
	 (multiple-value-bind (first-state last-state)
	     (add-triphone-seq-to-hmm :hmm hmm :triphone-seq triphones :source-state prev-last-state :word word :word-final t :log-base log-base)
	   (declare (ignorable first-state))	   
	   (setf prev-last-state last-state)
	   (when add-optional-silences (add-triphone-to-hmm :hmm hmm :triphone sil-phone :word "<sil>" :word-final t :log-base log-base :source-state prev-last-state :dest-state prev-last-state))))
    ;; Connect to state #2 (where the closing silence is).
    (connect-lang-hmm-states prev-last-state 2 hmm 0.0)))
	   
(defun build-align-hmm-with-alt-pron (words hmm &key log-base (add-optional-silences t))
  "Build an alignment HMM, allow each word to be pronounced in any of the alternate ways that are in the dictionary.
   Allow optional silences between words, if specified.
   This could be a little more sophisticated.... but it's not a priority that it is."
  (setf words (coerce words 'vector))
  (let ((prev-last-state 1) ;; previous last state is state #1 (after the opening silence).
	(sil-phone (get-matching-triphones *acoustic-model* :base "SIL" :singleton t)))
    (loop for word across words do
      (unless (get-phonemes-for-word word) (error "Can't align.  Word '~A' not in dictionary!" word)))
    ;; Allow inserted optional silences up front
    (when add-optional-silences (add-triphone-to-hmm :hmm hmm :triphone sil-phone :word "<sil>" :word-final t :log-base log-base :source-state prev-last-state :dest-state prev-last-state))
    (loop for i from 0 below (length words)
       for word = (elt words i)
       for prev-word = (if (/= i 0) (elt words (1- i)) "<s>")
       for next-word = (if (/= i (1- (length words))) (elt words (1+ i)) "</s>")
       for prev-phone = (last-elt (get-phonemes-for-word prev-word))
       for next-phone = (first (get-phonemes-for-word next-word))
       for ending-states = '() then '()
       for pronunciations = (word->pronunciations word) do
	 (loop for pronunciation in pronunciations
	    for triphones =  (get-word-triphone-seq pronunciation :left prev-phone :right next-phone :singleton t) do
	      (multiple-value-bind (first-state last-state)
		  (add-triphone-seq-to-hmm :hmm hmm :triphone-seq triphones :source-state prev-last-state :word pronunciation :word-final t :log-base log-base)
		(declare (ignorable first-state))
		(when add-optional-silences (add-triphone-to-hmm :hmm hmm :triphone sil-phone :word "<sil>" :word-final t :log-base log-base :source-state first-state :dest-state first-state))
		(push last-state ending-states)))
	 (if (= (length ending-states) 1)
	     (setf prev-last-state (first ending-states))
	     (progn
	       (let ((new-end-state (get-new-language-hmm-state hmm)))
		 (dolist (state ending-states)
		   (connect-lang-hmm-states state new-end-state hmm 0.0))
		 (setf prev-last-state new-end-state))))
	 (when add-optional-silences (add-triphone-to-hmm :hmm hmm :triphone sil-phone :word "<sil>" :word-final t :log-base log-base :source-state prev-last-state :dest-state prev-last-state)))
    ;; Connect to state #2 (where the closing silence is).
    (connect-lang-hmm-states prev-last-state 2 hmm 0.0)))
