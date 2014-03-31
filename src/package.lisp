;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(cl-user::file-summary "Defines the sphinx-l package.")

;; This probably isn't the right place for this... but it's as good as any?!?
;;#-darwin
;;(port-audio::pa-initialize)

(defpackage :sphinx-l
  (:use :common-lisp :blambert-util :alexandria) ;; :bordeaux-fft :cl-ppcre )
  (:import-from :metatilities :defclass* :defclass-brief)
  ;; Alexandria has a 'variance' and 'mean' function that we have to shadow...
  (:shadow :variance :mean)
  (:export :dictionary-word-phoneme-map
	   :read-dictionary
	   ))





