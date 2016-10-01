;;;; Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

;;;; The display portion of a little text line-level meter.  Currently only working properly in *inferior-lisp* and on the command line

(defun display-meter (x &key (width 50))
  "Display the intial meter."
  (when (> x 1) (setf x 1))
  (when (< x 0) (setf x 0))
  (setf x (floor (* x width))) 
  (format t "|~A~A| (~a)" (make-string x :initial-element #\*) (make-string (- width x) :initial-element #\_) x)
  (terpri)
  (force-output)))

(defun update-meter (x &key (width 50))
  "Update the meter to a new level, without going to a new line."
  (format t "~c" #\Return);; go to the beginning of the line
  (display-meter x :width width))

(defun test-meter (&key (count 10))
  "Randomly generate numbers and display them on the meter to test it."
  (display-meter 0)
  (dotimes (i count)
    (update-meter (random 1.0))
    (sleep 1)))

#+port-audio
(defun live-level-meter (&key (seconds 10) (update-freq 0.5) (sample-rate 44100) (verbose t))
  "This is supposed to periodically output the total line level meter."
  (let ((segments (ceiling (/ seconds update-freq)))
	(samples-per-segment (truncate (* sample-rate update-freq)))
	(max-volume 100))
    (display-meter 0)
    (dotimes (i segments)
      (handler-case
	  (let* ((samples (port-audio:read-audio-samples samples-per-segment))	;; 22k is about 1/2 a second...
		 (sum (reduce (lambda (x y) (+ x (abs y))) samples))
		 (level (/ sum samples-per-segment)))
	    (declare (ignore level))
	    (update-meter (/ sum max-volume)))
	(error (e) (when verbose (format t "Skipping segment... ~A~%" e))))
      (force-output t))))
