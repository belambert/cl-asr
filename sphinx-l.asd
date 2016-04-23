;;-*- Mode: Lisp -*- 

;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

;; We've hopefully already loaded the shared object in .sbclrc?
;;(port-audio::pa-initialize)

(defsystem "sphinx-l"
  :description "A Lisp-based automatic speech recognition system"
  :version "0.3.0"
  :author "Ben Lambert"
  :serial t
  :components
  ((:module src
	    :serial t
	    :components ((:file "package")
			 (:file "vector-math")
			 (:file "global")
			 (:module fsm
			 	  :serial t
			 	  :components ((:file "fsm")
					       (:file "flat-bigram-lm")))
			 (:module hmm
			 	  :serial t
			 	  :components ((:file "gaussian")		 
					       (:file "data-structures")
					       (:file "language-hmm")
					       (:file "language-hmm-fsm")
					       (:file "new-language-hmm")
					       (:file "align")
					       (:file "flat")
					       (:file "lextree")
					       (:file "lextree-cd")
					       (:file "plot-language-hmm")
					       ))
			 (:module extensions
			 	  :serial t
			 	  :components (
					       ;;(:file "phoneme-recognition")
					       (:file "decoder-semantics")
					       (:file "visualization")
					       ;;(:file "generative")
					       ))
			 (:module signal-processing
			 	  :serial t
			 	  :components ((:file "cosine-transform")
					       (:file "feature-computation")))
			 (:module core
			 	  :serial t
			 	  :components ((:file "dictionary")
					       (:file "trellis")
					       (:file "main")))
			 (:module interface
			 	  :serial t
			 	  :components ((:file "wav-interface")
					       (:file "common")
					       (:file "gaussian")
					       (:file "mixw")
					       (:file "tmat")
					       (:file "lda")
					       (:file "mdef")
					       (:file "mfcc")
					       (:file "sphinx-interface")
					       (:file "sphinx-cli")
					       ))
			 ;; (:module lm
			 ;; 	  :serial t
			 ;; 	  :components ((:file "ngram-model-dmp")))
			 (:module standalone
			 	  :serial t
			 	  :components ((:file "meter")
			 		       (:file "model-io")
			 		       (:file "audio-io")
			 		       ;;(:file "asr-interface")
			 		       ))
			 ;; (:module training
			 ;; 	  :serial t
			 ;; 	  :components ((:file "training-fsm")
			 ;; 		       (:file "connected-training")
			 ;; 		       (:file "phoneme-model-training")))
			 )))

  :depends-on (:bordeaux-fft
	       :cl-ppcre
	       :sb-posix
	       :language-model
	       :port-audio
	       :mixalot-mp3
	       :ieee-floats
	       :alexandria
	       :metatilities
	       :array-operations
	       :cl-fad
	       :split-sequence
	       :gnuplot))


