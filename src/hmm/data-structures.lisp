;;;; Author: Ben Lambert
;;; ben@benjaminlambert

(in-package :sphinx-l)

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
