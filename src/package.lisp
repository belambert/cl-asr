;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

;; This probably isn't the right place for this... but it's as good as any?
;;#-darwin
;;(port-audio::pa-initialize)

(defpackage :sphinx-l
  ;;(:use :common-lisp :blambert-util :alexandria) ;; :bordeaux-fft :cl-ppcre )
  (:use :common-lisp :alexandria) ;; :bordeaux-fft :cl-ppcre )
  (:import-from :metatilities :defclass* :defclass-brief)
  ;; Alexandria has a 'variance' and 'mean' function that we have to shadow...
  (:shadow :variance :mean)
  (:export :dictionary-word-phoneme-map
	   :read-dictionary))
