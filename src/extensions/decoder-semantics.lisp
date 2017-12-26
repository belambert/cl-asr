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

(in-package :sphinx-l)

;;;; Early/simple try at doing ASR on arithmetic expressions

(defun digit->number (str-digit)
  "Convert a string version of a numeric digit between zero and ten, to the
   corresponding number (0-9)."
  (cond ((string-equal str-digit "zero") 0)
	((string-equal str-digit "one") 1)
	((string-equal str-digit "two") 2)
	((string-equal str-digit "three") 3)
	((string-equal str-digit "four") 4)
	((string-equal str-digit "five") 5)
	((string-equal str-digit "six") 6)
	((string-equal str-digit "seven") 7)
	((string-equal str-digit "eight") 8)
	((string-equal str-digit "nine") 9)
	((string-equal str-digit "ten") 10)
	(t nil)))

(defun plus-word-p (word)
  "Is the given word a 'plus' word.  I.e. is it either 'plus' or 'and'?"
  (or (string-equal word "plus")
      (string-equal word "and")))

(defun equal-word-p (word)
  "Is the given word an 'equal' word.  I.e. is it either 'equal' or 'are'?"
  (or (string-equal word "equal")
      (string-equal word "are")))

(defun valid-arithmetic-expression? (word-list)
  "Given a list of words, check if they comprise a valid arithmetic expression.
   Returns 1.0 if yes, -1.0 if no, and 0.0 if it's an incomplete arithmetic expression."
  (when (< (length word-list) 5)
    (return-from valid-arithmetic-expression? 0.0))
  (when (> (length word-list) 5)
    (setf word-list (subseq word-list (- (length word-list) 5) (length word-list))))
  (let* ((a (digit->number (elt word-list 0)))
	 (b (digit->number (elt word-list 2)))
	 (c (digit->number (elt word-list 4)))
	 (score (if (and a b c
			 (plus-word-p (elt word-list 1))
			 (equal-word-p (elt word-list 3)))
		    (if (= (+ a b) c)
			-1.0
			1.0)
		    0.0)))
    score))
