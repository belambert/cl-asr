;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Global variables defined up front")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; The global acoustic model ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "The global acoustic model")

(defvar *acoustic-model* nil
  "The currently loaded acoustic model which is comprised of individual phoneme models.")

(defvar *lm* nil
  "The currently loaded language model that is used for decoding.")

(defvar *phone-prefix* "+++PHONE+++"
  "This prefix designates a word as being actually a phone, not a true word.")





