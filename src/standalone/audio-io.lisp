;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com)

(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Our internal audio representation          ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (audio-segment
	     (:print-function
	      (lambda (struct stream depth)
                 (declare (ignore depth))
                 (format stream "[audio-segment: ~A seconds, sample-rate: ~A, sample-count: ~A]"
			 (audio-segment-seconds struct)
			 (audio-segment-sample-rate struct)
			 (length (audio-segment-samples struct))))))
  "Defines an audio segment.  Consists of samples and meta-information, like the source, sampling rate, etc.
   Can be serialized and save to a file."
  seconds
  id
  source
  sample-rate
  sample-resolution
  transcript
  samples
  filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; High-level audio recording and playback  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+port-audio
(defun record-audio (&key (seconds 2.0) (sample-rate 16000d0) (show-spectrogram nil) (transcript nil) (device-index 0))
  "Record audio using the specified parameter, and return the audio as  an 'audio-segment' struct."
  (let* ((total-sample-count (floor (* seconds sample-rate)))
	 (samples (port-audio:read-audio-samples total-sample-count :sample-rate sample-rate :device-index device-index))
	 (audio-segment (make-audio-segment :seconds seconds 
					    :sample-rate sample-rate
					    :source "Recorded in Lisp"
					    :samples samples
					    :transcript transcript)))
    (when show-spectrogram (show-spectrogram audio-segment))
    audio-segment))

#+port-audio
(defun record-and-save-audio (filename &key (seconds 2.0) (sample-rate 16000d0) (show-spectrogram nil) (transcript nil) (device-index 1) (convert-to-mfcc t))
  "Record some audio and save it directly to a file."
  (let ((audio (record-audio :seconds seconds :sample-rate sample-rate :show-spectrogram show-spectrogram :transcript transcript :device-index device-index)))
    (save-audio-file audio filename)
    (when convert-to-mfcc
      (convert-audio-to-mfcc-file filename))))

#+port-audio
(defun play-audio-file (filename)
  "Given the filename of an audio track, load it, and play it back."
  (let ((audio-segment (load-audio-file filename)))
    (play-audio audio-segment)))

#+port-audio
(defun play-audio (audio-segment)
  "Given an audio-segment, play it back."
  (let ((samples (audio-segment-samples audio-segment))
	(sample-rate (audio-segment-sample-rate audio-segment)))
    (handler-case	      
	(port-audio:write-audio-samples samples :sample-rate sample-rate)
      (error (e) (format t "Playback error: ~A~%" e)))))

#+port-audio
(defun play-mp3 (file)
  "Given an MP3 filename, play it to the standard audio output..?"
  ;; Samples are always (signed-byte 16)
  (multiple-value-bind (samples rate channels encoding)
      (mpg123:decode-mp3-file file)
    ;; Convert the samples to floating point numbers...
    (let* ((divisor (expt 2 (1- 16))))
      (setf samples (map 'vector (lambda (x) (coerce (/ x divisor) 'single-float)) samples)))
    (format t "Sample count:   ~:d~%" (length samples))
    (format t "Sampling rate: ~A~%" rate)
    (format t "# of channels: ~A~%" channels)
    (format t "Encoding(?):   ~A~%" encoding)
    ;; Try to play it...
    (port-audio:write-audio-samples samples :sample-rate (coerce rate 'double-float) :channel-count channels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Modify transcripts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number->word (number)
  "Given a number between 0 and 9, return the English string representation of that: e.g. ONE, TWO...."
  (handler-case
      (let ((number (if (stringp number)       ;; make sure it's a number
			(parse-integer number)
			number)))
	(cond ((= number 0)
	       "ZERO")
	      ((= number 1)
	       "ONE")
	      ((= number 2)
	       "TWO")
	      ((= number 3)
	       "THREE")
	      ((= number 4)
	       "FOUR")
	      ((= number 5)
	       "FIVE")
	      ((= number 6)
	       "SIX")
	      ((= number 7)
	       "SEVEN")
	      ((= number 8)
	       "EIGHT")
	      ((= number 9)
	       "NINE")
	      ((= number 10)
	       "TEN")
	      (t (format t "Unable to convert: ~A~%" number)
		 number)))
    (error (e) (declare (ignore e))(progn 
				     (format t "Unable to convert: ~A~%" number)
				     number))))

(defun replace-numeric-with-word-number (transcript)
  "Given a transcript string or list, replace all the numeric numbers with English string versions...
   i.e. 0 -->  ZERO, etc."
  (unless (listp transcript)
    (setf transcript (cl-ppcre:split "\\\s+" transcript)))
  (let ((new-script '()))
    (dolist (word transcript)
      (setf word (number->word word))
      (push word new-script))
    (nreverse new-script)))

(defun process-audio-transcripts (folder function)
  "Call the given function on the transcripts of all the .audio files in
   the  given folder."
  (let ((files (cl-fad:list-directory  folder)))
    (setf files (remove-if-not (lambda (x) (cl-ppcre:scan ".audio$" x)) files))
    (dolist (file files)
      (let* ((filename (format nil "~A/~A" folder file))
	     (audio (load-audio-file filename))
	     (transcript (audio-segment-transcript audio))
	     (new-transcript (funcall function transcript)))
	(format t "Changing transcript:~%     ~A~%     ~A~%" transcript new-transcript)
	(force-output t)
	(setf (audio-segment-transcript audio) new-transcript)
	(save-audio-file audio filename)))))

(defun process-mfc-audio-transcripts (folder function)
  "Call the given function on the transcripts of all the .audio files in
   the  given folder."
  (let ((files (cl-fad:list-directory  folder)))
    (setf files (remove-if-not (lambda (x) (cl-ppcre:scan ".mfc$" x)) files))
    (dolist (file files)
      (let* ((filename (format nil "~A/~A" folder file))
	     (audio (load-mfcc-audio filename))
	     (transcript (mfcc-sequence-transcript audio))
	     (new-transcript (funcall function transcript)))
	(format t "Changing transcript:~%     ~A~%     ~A~%" transcript new-transcript)
	(force-output t)
	(setf (mfcc-sequence-transcript audio) new-transcript)
	(save-object audio filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Audio already represented as MFCC features ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-mfcc-audio (filename)
  "Load an MFCC audio file.  MFCC features have already been computed."
  (let* ((mfcc (load-object filename))
	 (mfccs (mfcc-sequence-mfcc-seq mfcc)))
    (setf mfccs (map 'vector (lambda (x) (coerce x '(vector single-float))) mfccs))
    (when (stringp (mfcc-sequence-transcript mfcc))
      (setf (mfcc-sequence-transcript mfcc) (cl-ppcre:split " +" (mfcc-sequence-transcript mfcc))))
    (setf (mfcc-sequence-mfcc-seq mfcc) mfccs)
    (setf (mfcc-sequence-filename mfcc) filename)
    mfcc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; AUDIO READ-WRITE-SAVING AND LOADING  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-audio-file (audio filename)
  "Save an audio segment to a very basic, inefficient ASCII text file.
   This is a sphinx-l specific file format."
  (with-open-file (file-stream filename :direction :output :if-exists :supersede)
    (format file-stream "~S~%" (audio-segment-seconds audio))
    (format file-stream "~S~%" (audio-segment-id audio))
    (format file-stream "~S~%" (audio-segment-source audio))
    (format file-stream "~S~%" (audio-segment-sample-rate audio))
    (format file-stream "~S~%" (audio-segment-sample-resolution audio))
    (format file-stream "~S~%" (audio-segment-transcript audio))
    (format file-stream "~S~%" (audio-segment-samples audio))))

(defun save-audio-to-file (audio filename)
  "Synonym for the previous function."
  (save-audio-file audio filename))

(defun load-audio-file (filename)
  "Load an audio segment from a file.  This is a sphinx-l specific file format."
  (let ((audio-segment (make-audio-segment)))
    (with-open-file (file-stream filename :direction :input)
      (setf (audio-segment-seconds audio-segment) (read-from-string (read-line file-stream)))
      (setf (audio-segment-id audio-segment) (read-from-string (read-line file-stream)))
      (setf (audio-segment-source audio-segment) (read-from-string (read-line file-stream)))
      (setf (audio-segment-sample-rate audio-segment) (read-from-string (read-line file-stream)))
      (setf (audio-segment-sample-resolution audio-segment) (read-from-string (read-line file-stream)))
      (setf (audio-segment-transcript audio-segment)(read-from-string (read-line file-stream)))
      (setf (audio-segment-samples audio-segment) (read file-stream nil))) ;; the last one is a little different, because it's not all on one line...
    (setf (audio-segment-filename audio-segment) filename)
    audio-segment))
