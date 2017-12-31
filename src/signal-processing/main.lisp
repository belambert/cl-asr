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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  Feature computation: convert audio files to MFCC files  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
