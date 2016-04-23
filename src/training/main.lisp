;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Training acoustic models (word or phoneme models)   ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun train-isolated-word-models (training-data-dir new-model-dir &key (iterations 10) (pruning-threshold nil))
  "Main user function for training word models."
  (train-from-connected-words training-data-dir nil new-model-dir iterations 
			       :save-intermediate-models nil
			       :evaluate-intermediate-models nil
			       :evaluation-data-folder nil
			       :pruning-threshold pruning-threshold))

(defun train-connected-word-models (training-data-dir initial-model-dir new-model-dir &key 
				    (iterations 10) (pruning-threshold nil) (silence-penalty 100.0))
  "Main user function for training word models."
  (train-from-connected-words training-data-dir initial-model-dir new-model-dir iterations 
			       :save-intermediate-models nil
			       :evaluate-intermediate-models nil
			       :evaluation-data-folder nil
			       :pruning-threshold pruning-threshold
			       :silence-penalty silence-penalty))

(defun train-phoneme-models-user (foldername initial-model-dir new-model-dir iterations dictionary-filename 
				  &key (ctl-file nil) (transcript-file nil) 				   
				  save-intermediate-models evaluate-intermediate-models evaluation-data-folder 
				  (silence-penalty 100.0))
  "Main user function for training phoneme models"
  (when transcript-file (error "Transcript file option not yet supported."))
  (let ((ctl-list (if ctl-file
		      (file->line-list ctl-file)
		      (remove-if-not (lambda (x) (cl-ppcre:scan ".mfc$" x))(list-directory foldername)))))
    (format t "Training phoneme models on ~A audio files.~%" (length ctl-list))
    (train-phoneme-models foldername initial-model-dir new-model-dir iterations dictionary-filename
			  :file-list ctl-list
			  :save-intermediate-models save-intermediate-models 
			  :evaluate-intermediate-models evaluate-intermediate-models 
			  :evaluation-data-folder evaluation-data-folder
			  :silence-penalty silence-penalty)))
