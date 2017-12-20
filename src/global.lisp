;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com


;;;; The global acoustic model


(defvar *acoustic-model* nil
  "The currently loaded acoustic model which is comprised of individual phoneme models.")

(defvar *lm* nil
  "The currently loaded language model that is used for decoding.")

(defvar *phone-prefix* "+++PHONE+++"
  "This prefix designates a word as being actually a phone, not a true word.")
