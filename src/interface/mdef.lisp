;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Reading Sphinx model definition files.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read model definition file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Read model definition file")

(defun integer-first-token (string)
  "Given a string, get the first token (defined as space-separated),
   and convert that token to an integer."
  (parse-integer (first (split-sequence:split-sequence #\Space string))))

(defun convert-position (s)
  "Convert the Sphinx code for a phoneme 'position' into a keyword."
  (cond ((equalp s "b")
	 :begin)
	((equalp s "i")
	 :internal)
	((equalp s "e")
	 :end)
	((equalp s "s")
	 :single-word)
	(t nil)))

(defstruct (triphone (:print-function (lambda (struct stream depth)
					(declare (ignore depth))
					(print-unreadable-object (struct stream)
					  (format stream "triphone ~A" (make-triphone-readable struct))))))
  "A basic structure to represent a triphone, immediately after being
  read from the mdef file."
  base        ;; base phone
  left        ;; left context
  right       ;; right context
  position    ;; word internal, word beginning, word ending, single-word triphone
  attrib      ;; either 'filler' or 'n/a'
  )

(defstruct (triphone-model (:include triphone))
  "A basic structure to represent a triphone, immediately after being
  read from the mdef file."
  tmat        ;; an integer index into the sequence of transition matricies
  state-ids)  ;; a list of the integer indicies of each state's GMM


(defun read-mdef-verbose (&key n1 n-base n-tri n-state-map n-tied-state n-tied-ci-state n-tied-tmat)
  "Print summary information while reading in a Sphinx model definition file."
  (write-line "**************************************************")
  (write-line "Model definition summary:")
  (format t "Mystery number:      ~:D~%" n1)
  (format t "Base phoneme count:  ~:D~%" n-base)
  (format t "Base triphone count: ~:D~%" n-tri)
  (format t "State map count:     ~:D~%" n-state-map)
  (format t "Tied state count:    ~:D~%" n-tied-state)
  (format t "Tied CI state count: ~:D~%" n-tied-ci-state)
  (format t "Tied TMAT count:     ~:D~%" n-tied-tmat))

(defun read-mdef (filename &key verbose)
  "Read a Sphinx model definition file."
  (let* ((lines (delete-if (lambda (x) (or (= (length x) 0) (char-equal (elt x 0) #\#)))
			   (bl:file->line-list filename)))
	 (n1 (read-from-string (pop lines)))
	 (n-base (integer-first-token (pop lines)))
	 (n-tri (integer-first-token (pop lines)))
	 (n-state-map (integer-first-token (pop lines)))
	 (n-tied-state (integer-first-token (pop lines)))
	 (n-tied-ci-state (integer-first-token (pop lines)))
	 (n-tied-tmat (integer-first-token (pop lines)))
	 (line-count (length lines))
	 (triphones '()))
    ;; The number of lines left is the number of triphones, plus the number of phonemes
    (assert (= line-count (+ n-base n-tri)))
    (assert (= n-state-map (* line-count 4)))
    (when verbose (read-mdef-verbose :n1 n1 :n-base n-base :n-tri n-tri :n-state-map n-state-map :n-tied-state n-tied-state :n-tied-ci-state n-tied-ci-state :n-tied-tmat n-tied-tmat))
    (dolist (line lines)
      ;; Most of the consing is happening in this SPLIT-SEQUENCE call
      (multiple-value-bind (base left right position attrib tmat s1 s2 s3 n)
	  (values-list (split-sequence:split-sequence #\Space line :remove-empty-subseqs t))
	(assert (find position '("b" "i" "e" "s" "-") :test 'equalp))
	(assert (or (string-equal attrib "n/a") (string-equal attrib "filler")))
	(setf tmat (parse-integer tmat))
	(setf s1 (parse-integer s1))
	(setf s2 (parse-integer s2))
	(setf s3 (parse-integer s3))
	(assert (string-equal n "N"))
	(when (string-equal left "-") (setf left nil))
	(when (string-equal right "-") (setf right nil))
	(push (make-triphone-model :base base :left left :right right :position (convert-position position) :attrib attrib
			     :tmat tmat
			     :state-ids (list s1 s2 s3))
	       triphones)))
    (values (nreverse triphones) n-tied-state)))



