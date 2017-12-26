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

(defun build-flat-bigram-lang-hmm-from-vocab (vocab &key (model-source :phone) (transition-score-function 'negative-log) word-insertion-penalty silence-penalty)
  "Build a language HMM from a bigram LM and a model directory (of word/phoneme models?)."
  (declare (ignore transition-score-function))
  (let* ((fsm (build-flat-bigram-fsm vocab))
	 (lang-hmm (create-language-hmm-from-fsm fsm
						 ;; This part is probably broken...
						 ;;(acoustic-model-phone-hmm-table sphinx-l::*acoustic-model*)
						 *acoustic-model*
						 :model-source model-source
						 ;;:transition-score-function transition-score-function
						 :silence-penalty silence-penalty
						 :insertion-penalty word-insertion-penalty)))
    lang-hmm))

(defun build-flat-bigram-fsm (vocab)
  "Given a vocabulary, build a bigram FSM.  This FSM has leading and trailing silences.
   The FSM has only 4 states: edge 0-1 is silence, as is edge 2-3.  Each word in the vocab is placed
   on its own edge from state 1 to state 2.  There is an epsilon edge from state 2 to state 1 so we
   can recognize more than one word between the leading and trailing silences."
  (let* ((state-count 4)
	 (node-list '())
	 (edge-list '())
	 (start-state 0)
	 (end-state (1- state-count))
	 (edge-count 0))
    ;; ***************************************************************
    ;; create the nodes... just 4 though!  words are on the edges!
    (dotimes (i state-count)
      (push (make-fsm-node :id i :lang-hmm-id i) node-list))
    ;; ***************************************************************
    ;; Create an edge from state 0 to state 1, labeled with a silence
    (push (make-fsm-edge :id edge-count
			 :source-id 0
			 :destination-id 1
			 :label "<sil>") edge-list)
    (incf edge-count)
    ;; ***************************************************************
    ;; Create an edge for each word in the vocab
    (push "<sil>" vocab)
    (setf vocab (remove "<s>" vocab :test 'string-equal))
    (setf vocab (remove "</s>" vocab :test 'string-equal))
    ;;(setf vocab (remove-duplicates vocab :test 'string-equal))
    (setf vocab (remove-duplicates-fast vocab :ht-test 'equalp))
    (dolist (word vocab)
      (assert word) ;; make sure it's non nil
      (let ((edge (make-fsm-edge :id edge-count
				 :source-id 1
				 :destination-id 2
				 :label word)))
	(push edge edge-list)
	(incf edge-count)))
    ;; ***************************************************************
    ;; Create a loop back from state #2 to state #1
    (push (make-fsm-edge :id edge-count
			 :source-id 2
			 :destination-id 1
			 :label nil) edge-list)
    (incf edge-count)
    ;; ***************************************************************
    ;; The add the last edge, the only one into the final state, which is a silence
    (push (make-fsm-edge :id edge-count
			 :source-id 2
			 :destination-id 3
			 :label "<sil>") edge-list)
    (incf edge-count)
    (assert (= edge-count (length edge-list)))
    ;; ***************************************************************
    ;; Create the FSM object! 
    (make-fsm :state-count state-count
	      :start-state start-state
	      :terminal-states (list end-state)
	      :edge-list (nreverse edge-list)
	      :node-list (nreverse node-list)
	      :edge-count edge-count
	      :vocab vocab)))
