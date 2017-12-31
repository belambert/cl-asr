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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Creation of language HMMs   ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun compose-phoneme-hmm-for-training-example-for-bootstrapping (mfcc-seq models &key (pad-with-silence nil) silence-penalty)
  "Given some audio and a hash-table of word-hmms, construct a simple language 
   HMM to that may be used for training/segmentation.
   THIS ONE IS SIMPLIFIED, AND JUST USED FOR BOOTSTRAPPING... POSSIBILY CAN BE DEPRECATED LATER ON."
  (let* ((word-seq (if (stringp (mfcc-sequence-transcript mfcc-seq))
		       (cl-ppcre:split "\\\s+" (mfcc-sequence-transcript mfcc-seq))
		       (mfcc-sequence-transcript mfcc-seq)))
	 (phoneme-seq (word-sequence->phoneme-sequence word-seq))
	 (fsm (create-fsm-from-word-seq phoneme-seq :insert-optional-silences-between-words nil :mandatory-begin-and-end-silence pad-with-silence))
	 (lang-hmm (create-language-hmm fsm models :model-source :word :silence-penalty silence-penalty))) ;; make it pretend they are words!?!
    lang-hmm))

(defun compose-phoneme-hmm-for-training-example (mfcc-seq models &key (pad-with-silence t) (insert-optional-silences-between-words t) silence-penalty)
  "Given some audio and a hash-table of word-hmms, construct a simple language 
   HMM to that may be used for training/segmentation.
   THIS ONE WE USE FOR THE MAIN PHONEME TRAINING PHASE"
  (let* ((word-seq (if (stringp (mfcc-sequence-transcript mfcc-seq))
		       (cl-ppcre:split "\\\s+" (mfcc-sequence-transcript mfcc-seq))
		       (mfcc-sequence-transcript mfcc-seq)))
	 (fsm (create-fsm-from-word-seq word-seq 
					:insert-optional-silences-between-words insert-optional-silences-between-words
					:mandatory-begin-and-end-silence pad-with-silence))
	 (lang-hmm (create-language-hmm fsm models :model-source :phone :silence-penalty silence-penalty)))
    lang-hmm))
