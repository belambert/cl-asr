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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Regression test: make sure program updates haven't broken anything   ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decoder-regression-test (&key verbose)
  "Run a few test on the recogntion of digits."
  (format t "WORD MODELS:~%")
  (format t "Isolated digits, with single-word grammar:~%")
  (format t "Training set:~%")(force-output t)
  (evaluate-on-test-set "./data/audio/digits-train/" nil verbose :grammar-filename "./misc/grammars/isolated-digit.fsm"
			:model-directory "./misc/models/digits/iso-digit-models/" :phone-or-word :word)
  (format t "Test set:~%")(force-output t)
  (evaluate-on-test-set "./data/audio/digits-test/" nil verbose :grammar-filename "./misc/grammars/isolated-digit.fsm" 
			:model-directory "./misc/models/digits/iso-digit-models/" :phone-or-word :word)
  (format t "Isolated digits, unrestricted grammar:~%")
  (format t "Training set:~%")(force-output t)
  (evaluate-on-test-set "./data/audio/digits-train/" nil verbose :grammar-filename "./misc/grammars/digits.fsm" 
			:model-directory "./misc/models/digits/iso-digit-models/" :phone-or-word :word)
  (format t "Test set:~%")(force-output t)
  (evaluate-on-test-set "./data/audio/digits-test/" nil verbose :grammar-filename "./misc/grammars/digits.fsm" 
			:model-directory "./misc/models/digits/iso-digit-models/" :phone-or-word :word)
  (format t "Phone numbers, with ph# grammar:~%")(force-output t)
  (evaluate-on-test-set "./data/audio/phone-numbers/"  nil verbose :grammar-filename "./misc/grammars/phone-number-grammar.fsm" 
			:model-directory "./misc/models/digits/connected-word-models" :phone-or-word :word)
  (format t "Phone numbers, with unrestricted grammar:~%")(force-output t)
  (evaluate-on-test-set "./data/audio/phone-numbers/" nil verbose :grammar-filename "./misc/grammars/phone-number-grammar.fsm" 
			:model-directory "./misc/models/digits/connected-word-models" :phone-or-word :word))

(defun connected-word-model-test (&key (verbose t) (iterations 10) (pruning-threshold nil))
  "Test the connected word model training... Uses my recording of connected digits (1,2,3...)."
  (train-connected-word-models "./data/audio/connected-digits"
			       "./misc/models/iso-digit-models/"
			       "./misc/models/connected-word-models/" 
			       :iterations iterations 
			       :pruning-threshold pruning-threshold
			       :silence-penalty 100.0)
  (format t "Evaluating new models with unrestricted digits grammar:~%")
  (evaluate-on-test-set "./data/audio/phone-numbers/" nil verbose :grammar-filename "./misc/grammars/digits.fsm" 
			:model-directory "./misc/models/connected-word-models/" :phone-or-word :word))
  
(defun test-phoneme-models (&key (verbose t) (iterations 10))
  "Train phoneme models from connected speech... Uses my recording of connected digits (1,2,3...)."
  (train-phoneme-models-user "./data/audio/connected-digits/"
			     "./misc/models/aurora-initial-phoneme-models/"
			     "./misc/models/connected-phoneme-models"
			     iterations
			     "./misc/dict/numbers.dict" 
			     ;; "./misc/dict/an4.dic"
			     :silence-penalty 100.0)
  (evaluate-on-test-set "./data/audio/phone-numbers/" nil verbose 
			:grammar-filename "./misc/grammars/digits.fsm"
			:model-directory "./misc/models/connected-phoneme-models"
			:phone-or-word :phone
			:dictionary "./misc/dict/numbers.dict"))
