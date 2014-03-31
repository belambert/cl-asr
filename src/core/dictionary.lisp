;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Reading and representation of a pronunciation dictionary")


(cl-user::section "Pronunciation dictionary")

(defvar *current-dictionary* nil
  "A global variable representation the current dictionary.
   The dictionary is represented by the struct below.")

(defstruct dictionary
  "Represents a pronunciation dictionary.  Has the original filename, a list of phonemes used, and a
   hash table mapping from words (case-insensitive) to lists of phonemes."
  source-filename
  phoneme-list
  word-phoneme-map
  word-pronunciations-map
  ) ;;(e.g. 'ONE' --> '('W' 'AH' 'N')

(defun get-phonemes-for-word (word)
  "Given a word, the the corresponding sequence of phonemes."
  (let ((phoneme-list (gethash word (dictionary-word-phoneme-map *current-dictionary*))))
    (unless phoneme-list
      (when (or (string-equal word "<s>") (string-equal word "</s>") (string-equal word "<\s>") (string-equal word "<sil>"))
	(return-from get-phonemes-for-word (list "SIL")))
      (error "NO PHONEMES FOUND IN DICTIONARY FOR WORD: ~A~%" word))
    phoneme-list))

(defun word->pronunciations (word)
  "Given a word, the the corresponding sequence of phonemes."
  (let ((pronunciations (gethash word (dictionary-word-pronunciations-map *current-dictionary*))))
    (unless pronunciations
      (error "NO PRONUNCIATIONS FOUND IN DICTIONARY FOR WORD: ~A~%" word))
    pronunciations))

(defun get-dict-vocab ()
  "Given a word, the the corresponding sequence of phonemes."
  (alexandria:hash-table-keys (dictionary-word-phoneme-map *current-dictionary*)))

(defun loaded-dictionary-size ()
  "Return the number of words in the currently loaded dictionary."
  (length (get-dict-vocab)))

(defun alt-pron-p (string)
  "Check if the given string represents an alternate pronunciation of a bas word.
   This won't work when the number is greater than 9."
  (declare (optimize (speed 3))
	   (simple-string string))
  (let ((length (length string)))
    (not (or (< length 3)
	     (char/= (elt string (- length 1)) #\))
	     (char/= (elt string (- length 3)) #\()
	     (not (digit-char-p (elt string (- length 2))))))))

(defun remove-alt-pron-marker (string)
  "This won't work when the number is greater than 9."
  (declare (optimize (speed 3))
	   (simple-string string))
  (if (alt-pron-p string)
      (subseq string 0 (- (length string) 3))
      string))

(defun read-dictionary (filename &key (force t))
  "Read a pronunciation dictionary into a global variable.  Option 'force' means to re-load the dictionary
   even if it's already loaded."
  (when (and *current-dictionary*
	     (equal (dictionary-source-filename *current-dictionary*) filename)
	     (not force))
    (return-from read-dictionary t))
  (format t "Reading pronunciation dictionary ~A...~%" filename)(force-output t)

  (let ((dictionary (make-hash-table :test 'equalp));; :size 1000000))
	(pronunciation-map (make-hash-table :test 'equalp))
	(phone-table (make-hash-table :test 'equalp))
	(counter 0))
    ;;; Setup the "word" <sil>
    (setf (gethash "<sil>" dictionary) '("SIL"))
    (setf (gethash "SIL" phone-table) t)
    (do-lines (line filename)
      (incf counter)
      (unless (cl-ppcre:scan "^;;.*" line)
	(let* ((tokens (cl-ppcre:split "\\\s+" line))
	       (word (first tokens))
	       (phone-seq (rest tokens))
	       (word-base nil)
	       )
	  (setf word (remove #\: word))
	  (setf word-base (remove-alt-pron-marker word))
	  (setf (gethash word dictionary) phone-seq)
	  ;;(push (list word phone-seq) (gethash word-base pronunciation-map))
	  (push word (gethash word-base pronunciation-map))

	  (dolist (phoneme phone-seq)
	    (setf (gethash phoneme phone-table) t)))))
    (format t "Finished reading dictionary.~%")(force-output t)    
    (let* ((phone-list (sort (mapcar 'first (hash-table->list phone-table)) 'string-lessp))
	   (dict (make-dictionary :source-filename filename
				  :phoneme-list phone-list
				  :word-phoneme-map dictionary
				  :word-pronunciations-map pronunciation-map)))
      (setf *current-dictionary* dict))))

(defun word-sequence->phoneme-sequence (word-seq)
  "Given a word sequence, convert that into a sequence of phonemes."
  (let ((phonemes '()))
    (dolist (word word-seq)
      (setf phonemes (append phonemes (get-phonemes-for-word word))))
    phonemes))
