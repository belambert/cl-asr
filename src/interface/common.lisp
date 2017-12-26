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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-header-history (history header-marker)
  "Given some stream history (which is reversed), and the designated header marker,
   check to see if the history ends with the header marker.  This is an non-consing function."
  (loop for x in history
     for y in header-marker do
       (when (char-not-equal x y)
	 (return-from check-header-history nil)))
  ;; If we made it though, then it's a match, return true.
  t)

(defun find-header-end (stream &key (header-marker "endhdr"))
  "Given a character stream, step through it until the end of header marker is found."
  (setf header-marker (nreverse (coerce header-marker 'list)))
  (let ((history '()))
    (loop for char = (read-char stream)
       until (equal char nil)
       for i from 0 below (file-length stream) do
	 (push char history)
	 (when (check-header-history history header-marker)
	   ;; Reset the file pointer to the beginning of the file..
	   (file-position stream 0)
	   (return-from find-header-end (+ i 2)))) ;; plus 2, one for the off-by-one-ness, the other for the newline character.
    nil))

(defun compute-checksum (data)
  "An attempt to replicate Sphinx's checksum computation -- however, this currently fails to compute the correct checksum."
  (reduce (lambda (sum x) (declare (type fixnum sum x)) (+ (logior (mod (ash sum 20) 2147483648) (ash sum -12)) x)) data))

(defun get-header-and-offset (filename &key verbose)
  "Given the filename of a Sphinx3 binary file, located the header, read it, and return 
   the header length and the header itself.  This opens and closes the file."
  (let ((header-length nil))
    (declare ((or fixnum null) header-length))
    (with-open-file (f filename :direction :input)
      (setf header-length (find-header-end f))
      (let* ((header (make-string header-length)))
	(read-sequence header f)
	(when verbose
	  (format t "HEADER:~A~%" header)
	  (format t "HEADER OFFSET: ~D~%" header-length))
	(values header-length (search "checksum0 yes" header) header)))))

(defun int->byte-chars (int)
  "Return an int represented by a 4 character string."
  (let ((chars '()))
    (dotimes (i 4)
      (push (code-char (ldb (byte 8 (* i 8)) int)) chars))
    (coerce (nreverse chars) 'string)))

(defun byte-swap (i)
  "Swap the endianness of a 32 bit number."
  (rotatef (ldb (byte 8 0) i) (ldb (byte 8 24) i))
  (rotatef (ldb (byte 8 8) i) (ldb (byte 8 16) i))
  i)
