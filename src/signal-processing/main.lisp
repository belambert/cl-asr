;;;; Author: Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Top level functions for performing feature extraction from audio")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Feature computation: convert audio files to MFCC files  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Feature computation: convert audio files to MFCC files")

(defun convert-folder-to-mfcc-simple (foldername &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13) (preemphasize 0.95)
				      (cepstra-mean-normalization t) (frame-length 25) (frame-delta 10) (extension ".mfc") (filename-transcript-function nil))
  "Convert all the audio or wav files in a directory into MFCC files."
  (let ((files (list-directory foldername)))
    (setf files (remove-if-not (lambda (x) (or (cl-ppcre:scan ".audio$" x)
					       (cl-ppcre:scan ".wav$" x))) files))
    (dolist (file files)
      (let ((filename (format nil "~A/~A" foldername file)))
	(format t "Converting file: ~A~%" file)(force-output t)
	(convert-audio-to-mfcc-file filename :filter-bank-size filter-bank-size :start-freq start-freq :end-freq end-freq 
				    :cepstrum-count cepstrum-count :preemphasize preemphasize :cepstra-mean-normalization cepstra-mean-normalization 
				    :frame-length frame-length :frame-delta frame-delta :extension extension 
				    :filename-transcript-function filename-transcript-function)))
    (length files)))


(defun convert-folder-to-mfcc (foldername &key (filter-bank-size 40) (start-freq 100) (end-freq 4000) (cepstrum-count 13) (preemphasize 0.95)
			       (cepstra-mean-normalization t) (frame-length 25) (frame-delta 10) (extension ".mfc") (ctl-file nil) (transcript-file nil))
  "Convert all the audio or wav files in a directory into MFCC files."
  (let ((transcript-table (when transcript-file (language-model::load-sphinx-transcript transcript-file)))
	(files (if ctl-file 
		      (file->line-list ctl-file)
		      (list-directory foldername))))
    (setf files (remove-if-not (lambda (x) (or (cl-ppcre:scan ".audio$" x)
					       (cl-ppcre:scan ".wav$" x))) files))
    (dolist (file files)
      (let ((filename (format nil "~A/~A" foldername file))
	    (transcript (when transcript-table (gethash (language-model::get-example-name-from-filename file) transcript-table))))
	(format t "Converting file: ~A~%" filename)(force-output t)
	(convert-audio-to-mfcc-file filename :filter-bank-size filter-bank-size :start-freq start-freq :end-freq end-freq 
				    :cepstrum-count cepstrum-count :preemphasize preemphasize :cepstra-mean-normalization cepstra-mean-normalization 
				    :frame-length frame-length :frame-delta frame-delta :extension extension
				    :transcript transcript)))
    (length files)))


