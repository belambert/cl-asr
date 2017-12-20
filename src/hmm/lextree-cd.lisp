;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creating a cons cell-based tree of phone specifiers -- this is essential ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vocab->triphone-lextree (vocab)
  "Given a list of words, convert it to a cons-cell based tree of phone (not tri-phone) specifiers.
   However, this tree includes sufficient redundancy that we'll be able to unambiguously convert each phone
   to a triphone later."
  (let* ((phone-lists (mapcar 'word->phone-list vocab)))
    (phone-lists->triphone-lextree phone-lists)))

(defun phone-lists->triphone-lextree (lists)
  "Given a list of 'phone lists', convert it to a cons-cell based tree of phone (not tri-phone) specifiers.
   However, this tree includes sufficient redundancy that we'll be able to unambiguously convert each phone
   to a triphone later."
  (let ((table (make-hash-table :test 'equalp)) ;; Has to be equalp because this time we're using lists as keys
	(tree '()))
    (loop for (first . rest) in lists 
       for phone-list in lists
       for prefix = (subseq phone-list 0 (min 2 (length phone-list))) do
	 (push rest (gethash prefix table)))
    (loop for root being the hash-keys of table using (hash-value subtree) do
	 (setf root (first root))
	 (if (and subtree (first subtree))
	     (push (cons root (phone-lists->triphone-lextree subtree)) tree)
	     (push root tree)))
    tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Constructing a basic version of a CD lextree lang hmm -- no fancy wrap-around, CD phones internal, CI phones at boundaries ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-cd-lextree-basic (vocab acoustic-model log-base)
  "Constructing a basic version of a lextree trellis -- no fancy wrap-around, CD phones internal, CI phones at boundaries."
  ;; Make sure we have a silence...
  (pushnew "<sil>" vocab :test 'string-equal)
  (let ((hmm (create-template-language-hmm acoustic-model log-base))
	(lextree (vocab->triphone-lextree vocab)))
    ;; There's no context dependency on the loopback, so we can do this:
    (connect-lang-hmm-states 2 1 hmm 0.0)
    (loop for subtree in lextree do
       (build-cd-lextree-recursive-basic subtree :hmm hmm :source-state 1))
    hmm))

(defun build-cd-lextree-recursive-basic (tree &key hmm source-state lc)
  "Recursive, helper function for the previous function."
  ;; This is the terminal case of the recusion: we got to the end of a word, so keep track of that, and return.
  (when (stringp tree)
    (set-state-word-final hmm source-state)
    (set-state-word hmm source-state tree)
    (connect-lang-hmm-states source-state 2 hmm 0.0)
    (return-from build-cd-lextree-recursive-basic))
  ;; Otherwise, add a triphone to the HMM, and recurse
  (let* ((root (car tree))
	 (subtree (cdr tree))
	 (base (string root))
	 (all-rc (unless (stringp (first subtree)) (mapcar 'first subtree)))
	 (rc (when all-rc (string (first all-rc))))
	 (phone (if (and base rc lc)
		    (get-matching-triphones *acoustic-model* :base (string base) :left (string lc) :right (string rc) :position :internal :singleton t)
		    (get-ci-triphone (string root)))))
    (when (> (length all-rc) 1)
      (assert (all-equal all-rc :test 'equalp)))
    (multiple-value-bind (start end)
	(add-triphone-to-hmm :hmm hmm :triphone phone :source-state source-state)
      (declare (ignore start))
      ;; Recurse
      (dolist (subsubtree subtree)
	(build-cd-lextree-recursive subsubtree :hmm hmm :source-state end :lc root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Constructing a true CD lextree lang hmm with CD phone everywhere (potentially) and context on wrap-around ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-cd-lextree (vocab acoustic-model log-base &key (loop-back :basic) left-ci right-ci)
  "Constructing a true CD lextree lang hmm with CD phone everywhere (potentially) and context on wrap-around."
  ;; Make sure we have a silence...
  (pushnew "<sil>" vocab :test 'string-equal)
  (let ((hmm (create-template-language-hmm acoustic-model log-base))
	(lextree (vocab->triphone-lextree vocab))
	(entry-table (make-hash-table :test 'equalp))
	(exit-table (make-hash-table :test 'equalp)))
    (loop for subtree in lextree do
	 (build-cd-lextree-recursive subtree :hmm hmm :entry-table entry-table :exit-table exit-table :left-ci left-ci :right-ci right-ci))
    ;; Connect states 1 and 2 to the middle states...
    (make-initial-connections hmm entry-table)
    (make-final-connections hmm exit-table)
    ;; Make the loop-back connections...
    (case loop-back
      (:basic (make-loop-back-connnections-basic hmm entry-table exit-table))
      (:full (make-loop-back-connnections-full hmm entry-table exit-table))
      (otherwise (error "Lookback method is required.")))
    hmm))

(defun add-first-triphone (base rc &key hmm source-state entry-table left-ci)
  "Helper function to make adding the first triphone(s) of a word a little easier."
  (setf rc (string rc))
  (setf base (string base))
  (when left-ci (setf rc nil))
  (let* ((phones (get-matching-triphones *acoustic-model* :base base :left nil :right rc :position :begin))
	 (destination-state (get-new-language-hmm-state hmm)))
    (add-parallel-triphones-to-hmm :hmm hmm :triphones phones :source-state source-state :dest-state destination-state :entry-table entry-table)))
    
(defun add-last-triphone (base lc word &key hmm source-state dest-state entry-table exit-table right-ci)
  "Helper function to make adding the last triphone(s) of a word a little easier."
  (setf lc (string lc))
  (setf base (string base))
  (when right-ci (setf lc nil))
  (let* ((phones (get-matching-triphones *acoustic-model* :base base :left lc :right nil :position :end)))
    (add-parallel-triphones-to-hmm :hmm hmm :triphones phones :source-state source-state :dest-state dest-state :word word :word-final t :entry-table entry-table :exit-table exit-table)))

(defun build-cd-lextree-recursive (tree &key hmm source-state lc entry-table exit-table left-ci right-ci)
  "Recursive, helper function the function above."
  ;; This is the terminal case of the recusion: we got to the end of a word, so keep track of that, and return.
  (when (stringp tree)
    (set-state-word-final hmm source-state)
    (set-state-word hmm source-state tree)
    (connect-lang-hmm-states source-state 2 hmm 0.0)
    (return-from build-cd-lextree-recursive))
  ;; Otherwise, add triphone(s) to the HMM, and recurse
  (let* ((root (car tree))
	 (subtree (cdr tree))
	 (base (string root))
	 (all-rc (unless (stringp (first subtree)) (mapcar 'first subtree)))
	 (rc (when all-rc (string (first all-rc))))
	 (word (when (stringp (first subtree)) (first subtree)))
	 (end-state nil))
    (when (> (length all-rc) 1)
      (assert (all-equal all-rc :test 'equalp)))
    (cond ((and (not lc) (not rc) word)
	   (add-last-triphone base lc word :hmm hmm :source-state source-state :dest-state nil :exit-table exit-table :entry-table entry-table :right-ci right-ci)
	   (return-from build-cd-lextree-recursive nil))
	  ((not lc)
	   (multiple-value-bind (start end)
	       (add-first-triphone base rc :hmm hmm :source-state nil :entry-table entry-table :left-ci left-ci)
	     (declare (ignore start))
	     (setf end-state end)))
	  ((and (not rc) word)
	   (add-last-triphone base lc word :hmm hmm :source-state source-state :dest-state nil :exit-table exit-table :right-ci right-ci)
	   (return-from build-cd-lextree-recursive nil))
	  ((and base rc lc)
	   (let ((phone (get-matching-triphones *acoustic-model* :base (string base) :left (string lc) :right (string rc) :position :internal :singleton t)))
	     (multiple-value-bind (start end)
		 (add-triphone-to-hmm :hmm hmm :triphone phone :source-state source-state)
	       (declare (ignore start))
	       (setf end-state end))))
	  (t (error "Impossibile situation.")))
    ;; Recurse
    (dolist (subsubtree subtree)
      (build-cd-lextree-recursive subsubtree :hmm hmm :source-state end-state :lc root :entry-table entry-table :exit-table exit-table :left-ci left-ci :right-ci right-ci))))
