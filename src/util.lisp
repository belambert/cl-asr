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

(defun truncate-list (list maximum-length)
  "Returns a subsequence of the list, with a max length as specified.  If the max length is null
   then returns the original list."
  (if maximum-length
      (subseq list 0 (min maximum-length
			  (length list)))
      list))

(defun text-file->line-list (filename &key (external-format :utf-8))
  "Quickly read an entire text file (UTF-8) into memory, and split it up by newlines."
  (with-open-file (file filename :direction :input :external-format external-format)
    (let ((char-array (make-array (the fixnum (file-length file)) :element-type 'character)))
      (read-sequence char-array file)	
      (split-string #\Newline char-array))))

(defun gzip-file->line-list (filename)
  "Open and decompress a gzipped file, then return a list of lines."
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let* ((gzip-length (file-length s))
	   (gzip-seq (make-array (the fixnum gzip-length) :element-type '(unsigned-byte 8))))
      (read-sequence gzip-seq s)
      (let* ((unzipped-seq (gzip-stream:gunzip-sequence gzip-seq))
	     (unzipped-length (length unzipped-seq))
	     (unzipped-string (make-array unzipped-length :element-type 'character)))
	(declare ((array (unsigned-byte 8)) unzipped-seq)
		 (simple-string unzipped-string))
	(map-into unzipped-string 'code-char unzipped-seq)
	(split-string #\Newline unzipped-string)))))

(defun file->line-list (filename &key (external-format :utf-8) (gzip nil gzip-supplied-p))
  "Read a file that may or may not be gzipped, and return a list of the lines.
   Automatically detects if it's a gzip file by the extension (at the end of the filename),
   but this can be overridden by supplying the argument :gzip."
  (let ((gzip-p (if (pathnamep filename)
		    (string-equal (pathname-type filename) "gz")
		    (alexandria:ends-with-subseq ".gz" filename :test #'char-equal))))
    (when gzip-supplied-p
      (setf gzip-p gzip))
    (if gzip-p
	(gzip-file->line-list filename)
	(text-file->line-list filename :external-format external-format))))

(defun remove-duplicates-fast (list &key (ht-test 'equalp) key)
  "Faster than the built in REMOVE-DUPLICATES on long lists.  Sacrifices memory use
   to speed up time, by creating an aux hash table.

   It's faster to just build a new list than to try to remove them from the original
   list... at least without some major bookkeeping..."
  (unless list
    (return-from remove-duplicates-fast list))
  (let ((ht (make-hash-table :test ht-test :size (length list)))
	(new-list '()))
    (loop for item in list
       for compare-item = (if key (funcall key item) item) do
	 (unless (gethash compare-item ht)
	   (push item new-list)
	   (setf (gethash compare-item ht) t)))
    (nreverse new-list)))

(defun nintersection-fast (list-1 list-2 &key (ht-test 'equalp))
  "Faster than the built-in INTERSECTION for long lists.  Creates a hash table to speed up
   comparisons.  ht-test is a HASHTABLE test, not an equality function operator.
   The current version is *not destructive*, and is the same as INTERSECTION-FAST."
  (let ((ht (make-hash-table :test ht-test))
	(new-list '()))
    (dolist (item list-1)
      (setf (gethash item ht) t))
    (dolist (item list-2)
      (when (gethash item ht)
	(push item new-list)))
    new-list))

(defun intersection-fast (list-1 list-2 &key (ht-test 'equalp))
  "Faster than the built-in INTERSECTION for long lists.  Creates a hash table to speed up
   comparisons.  ht-test is a HASHTABLE test, not an equality function operator."
  ;;(nintersection-fast (copy-seq list-1) list-2 :ht-test ht-test)
  (nintersection-fast list-1 list-2 :ht-test ht-test))
