;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "CI phone lextree decoding")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creating a cons cell-based tree of phone specifiers -- this is essential ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Creating a cons cell-based tree of phone specifiers -- this is essential.")

(defun word->phone-list (word)
  "Gets all the phones for a word, converts them to keywords, then appends
   the word to the end of the list.  We want the phones as keywords, so it's easy to tell the difference
   between a phone and the final word."
  (append (mapcar 'alexandria:make-keyword (get-phonemes-for-word word)) (list word)))
  
(defun vocab->lextree (vocab)
  "Given a list of list of words, convert it to a cons-cell based tree of phone (not tri-phone) specifiers."
  (let* ((phone-lists (mapcar 'word->phone-list vocab)))
    (phone-lists->lextree phone-lists)))
  
(defun phone-lists->lextree (lists)
  "Given a list of 'phone lists', convert them to a cons-cell based tree of phone (not tri-phone) specifiers."
  (let ((table (make-hash-table :test 'equalp)) ;; This will protect against duplicate *words* in the leaves
	(tree '()))
    (loop for (first . rest) in lists do
	 (push rest (gethash first table)))
    (loop for root being the hash-keys of table using (hash-value subtree) do
	 (if (and subtree (first subtree))
	     (push (cons root (phone-lists->lextree subtree)) tree)
	     (push root tree)))
    tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Do the actual construction of language HMM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Do the actual construction of language HMM")

(defun build-ci-lextree-hmm (vocab acoustic-model log-base)
  "Build a language HMM entirely out of CI phones, in a lex tree structure."
  ;; Make sure we have a silence...
  (pushnew "<sil>" vocab :test 'string-equal)
  (let ((hmm (create-template-language-hmm acoustic-model log-base))
	(lextree (vocab->lextree vocab)))
    ;; There's no context dependency on the loopback, so we can do this:
    (connect-lang-hmm-states 2 1 hmm 0.0)
    (loop for subtree in lextree do
       (build-ci-lextree-hmm-recursive subtree :hmm hmm :source-state 1))
    hmm))
    	 	
(defun build-ci-lextree-hmm-recursive (tree &key hmm source-state)
  "A helper function for the previous function"
  ;; This is the terminal case of the recusion: we got to the end of a word, so keep track of that, and return.
  (when (stringp tree)
    (set-state-word-final hmm source-state)
    (set-state-word hmm source-state tree)
    (connect-lang-hmm-states source-state 2 hmm 0.0)
    (return-from build-ci-lextree-hmm-recursive))  
  ;; Otherwise, add a triphone to the HMM, and recurse
  (let* ((root (car tree))
	 (subtree (cdr tree))
	 (phone (get-ci-triphone (string root))))
    ;; add a phone for root...
    (multiple-value-bind (start end)
	(add-triphone-to-hmm :hmm hmm :triphone phone :source-state source-state)
      (declare (ignore start))
      (dolist (subsubtree subtree)
	(build-ci-lextree-hmm-recursive subsubtree :hmm hmm :source-state end)))))

       



