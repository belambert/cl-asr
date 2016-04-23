;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

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
