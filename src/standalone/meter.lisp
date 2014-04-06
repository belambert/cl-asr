;;;; Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "The display portion of a little text line-level meter.  Currently only working properly in *inferior-lisp* and on the command line.")

(defun display-meter (x &key (max 50) (width 50))
  "Display the intial meter."
  (let ((delta (/ max width)))    
    (setf x (floor (* x delta)))
    (format t "delta=~a, x=~A, width=~A~%" delta x width)
    (format t "|~A~A|" (make-string x :initial-element #\*) (make-string (- width x) :initial-element #\_))
    (force-output)))

(defun update-meter (x &key (max 50) (width 50))
  "Update the meter to a new level, without going to a new line."
  (format t "~c" #\Return);; go to the beginning of the line
  (display-meter x :max max :width width))

(defun test-meter (&key (n 100) (count 10))
  "Randomly generate numbers and display them on the meter to test it."
  (display-meter 0 :max n)
  (dotimes (i count)
    (update-meter (random n) :max n)
    (sleep 1)))


;; Can I do this with a little ASCII "meter" ?? Using the linefeed character
;; See meter.lisp .... works in the shell... but not emacs on Mac..?
;;(defun live-level-meter (&optional (samples-per-segment 22050) (segments 20))   ;; default to about 10 seconds

#+port-audio
(defun live-level-meter (&key (seconds 10) (update-freq 0.5) (sample-rate 44100) (verbose t))
  "This is supposed to periodically output the total line level meter."
  (let ((segments (ceiling (/ seconds update-freq)))
	(samples-per-segment (truncate (* sample-rate update-freq)))
	;;(max-volume 0.1)
	(max-volume 1000)
	)
    (display-meter 0 :max max-volume)
    (dotimes (i segments)
      ;;(handler-case
	  (let* ((samples (port-audio:read-audio-samples samples-per-segment))	;; 22k is about 1/2 a second...
		 (sum (reduce (lambda (x y) (+ x (abs y))) samples))
		 (level (/ sum samples-per-segment)))
	    ;;(update-meter level :max max-volume)
	    ;;(update-meter sum :max max-volume)
	    (format t "Sum:   ~A~%" sum)
	    (format t "Level: ~A~%" level))
	;;(error (e) (when verbose (format t "Skipping segment... ~A~%" e))))
      (force-output t))))



