;;;; Author: Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :sphinx-l)

;;;; Some hacks that make it possible to recognize PHONES as opposed to WORDS, although quite awkwardly

(defun build-flat-phoneme-fsm (&key ci-only)
  "Build a flat bigram FSM for recognition by first constructing a pseudo vocab
   and using that as the list of words that can be recognized."
  (let ((pseudo-vocab (phonemes->pseudo-vocab :ci-only ci-only)))
    (build-flat-bigram-fsm pseudo-vocab)))

(defun phonemes->pseudo-vocab (&key ci-only)
  "Convert the phones in the currently loaded acoustic model into a list of pseudo-vocab."
  (let* ((phonemes (remove-if-not 'listp (hash-table-keys (acoustic-model-phone-hmm-table *acoustic-model*))))
	 (pseudo-vocab '()))
    (when ci-only
      (setf phonemes (remove-if 'second phonemes)))
    (setf pseudo-vocab (loop for phoneme in phonemes
			  for pseudo-word = (phone-specifier->string phoneme)
			  collecting pseudo-word))
    (push "<sil>" pseudo-vocab)))

(defun phone-specifier->string (phone-spec)
  "Given a phone specifier, a list of the phone possibly followed by before and after contexts,
   convert it to a canonical string to be used as a 'word' during recognition."
  (cond ((listp phone-spec)
	 (format nil "~A_~{~S~^_~}" *phone-prefix* phone-spec))
	((stringp phone-spec)
	 phone-spec)
	(t (error "Invalid phone specifier"))))

(defun phone-spec->pretty-string (phone-spec)
  "Given a phone specifier, a list of the phone possibly followed by before and after contexts,
   convert it to an easily human readable string."
  (cond ((listp phone-spec)
	 (if (second phone-spec)
	     (format nil "~A (~a, ~a), ~A" (first phone-spec) (second phone-spec) (third phone-spec) (fourth phone-spec))
	     (first phone-spec)))
	((stringp phone-spec)
	 phone-spec)
	(t (error "Invalid phone specifier"))))

(defun string->phone-spec (string)
  "Given a string representing the canonical 'word' version of a (possibly context dependent) phone,
   return a 'phone specifier' list."
  (mapcar 'read-from-string (subseq (split-sequence:split-sequence #\_ string) 1)))

(defun phone-string-p (string)
  "Check if the given string represents a phone (i.e. has the *phone-prefix* prefix)."
  (alexandria:starts-with-subseq *phone-prefix* string))

(defun cleanup-phone-string (string)
  "Given a string, *if* it represents a phone, then return the easily human readable version of it."
  (if (phone-string-p string)
      (phone-spec->pretty-string (string->phone-spec string))
      string))
