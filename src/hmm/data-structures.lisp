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

;;;; The class representation of an HMM (semi-deprecated)")

;; Will we still be using this...?
;; We're still using this in the FSM-based language HMM code, and training
(defclass* hmm ()
  ((word nil :type string)
   (left-context nil :type string)
   (right-context nil :type string)
   (position nil :type string)
   (state-count 0 :type fixnum)
   (emission-distributions #() :type (simple-array gaussian *))
   (transition-probabilities #2A(()) :type (simple-array single-float (* *)))
   (state-ids nil)
   (phoneme-state-ids nil))
  (:automatic-accessors t)
  (:automatic-initargs t)
  (:name-prefix "hmm" "-")
  (:documentation 
   "An HMM. -- for words and/or phonemes.  This is no longer a primary data structure,
   but we're still using this in the FSM-based language HMM code, and training"))
