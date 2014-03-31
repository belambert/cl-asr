;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Reading/writing WAV files")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Reading WAV files...  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Reading WAV files...")

(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given size.  Size is the number of BYTES."
  (ldb (byte size 0) value))

(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given size.  Size is the number of BYTES."
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

(defun bytes-to-integer (byte-array &optional swap-endianness)
  "Converts an array of bytes to an integer.  MUST BE LITTLE-ENDIAN!!"
  (when swap-endianness
    (setf byte-array (reverse byte-array)))
  (let ((int 0))
    (loop for byte across byte-array
       for i from 0 to (length byte-array)
       for offset = (* i 8) do
	 (setf (ldb (byte 8 offset) int) byte))
    int))

(defun integer-to-bytes (int &key (byte-count 4))
  (let ((bytes '()))
    (dotimes (i byte-count)
      (push (ldb (byte 8 (* i 8)) int)
	    bytes))
    (nreverse bytes)))

(defun byte-array-to-int-array (byte-array bits-per-int)
  "Convert a byte array into an int array.  Requires the number of bits per int to be specified.
   If the number of bits is 16, then each pair of bytes forms a single 16-bit integer."
  (assert (= (mod bits-per-int 8) 0))
  (let* ((bytes-per-int (coerce (/ bits-per-int 8) 'integer))
	 (int-array-length (/ (length byte-array) bytes-per-int))
	 (int-array (make-array int-array-length :element-type `(signed-byte ,bits-per-int))))
    (loop for int-offset from 0 below int-array-length
       for byte-offset = (* bytes-per-int int-offset) do
	 (setf (aref int-array int-offset) (unsigned-to-signed (bytes-to-integer (subseq byte-array byte-offset (+ byte-offset bytes-per-int))) bits-per-int)))
    int-array))

(defun int-array-to-byte-array (int-array bits-per-int)
  "Convert a byte array into an int array.  Requires the number of bits per int to be specified.
   If the number of bits is 16, then each pair of bytes forms a single 16-bit integer."
  (assert (= (mod bits-per-int 8) 0))
  (let* ((bytes-per-int (coerce (/ bits-per-int 8) 'integer))
	 (byte-array-length (* (length int-array) bytes-per-int))
	 (byte-array (make-array byte-array-length :element-type '(unsigned-byte 8)))
	 (int-array-length (length int-array)))
    ;; Do I need to make the int array consist of unsigned ints?!?!?!
    (loop for int across int-array
       for int-offset from 0 below int-array-length
       for byte-list = (integer-to-bytes int :byte-count bytes-per-int)
       for byte-array-offset1 = (* bytes-per-int int-offset) do
	 (loop for byte in byte-list
	    for i from 0 below bytes-per-int
	    for byte-array-offset2 = (+ byte-array-offset1 i) do
	      (setf (aref byte-array byte-array-offset2) byte)))
    byte-array))

;; ADD CHECK ON THE BYTE COUNTS.... ETC.
  
(defun load-wav-file (filename &key (verbose t))
  "Read a wav audio file. See http://www.sonicspot.com/guide/wavefiles.html.  Or: https://ccrma.stanford.edu/courses/422/projects/WaveFormat/"
  (when verbose (format t "Reading WAV file: ~A~%" filename))
  (let ((bytes nil))
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
      (setf bytes (make-array (file-length f) :element-type '(unsigned-byte 8)))
      (read-sequence bytes f))
    (assert (equal (map 'string 'code-char (subseq bytes 0 4)) "RIFF"))
    (assert (equal (map 'string 'code-char (subseq bytes 8 12)) "WAVE"))
    (assert (equal (map 'string 'code-char (subseq bytes 12 16)) "fmt "))
    (let ((subchunk2-size (- (bytes-to-integer (subseq bytes 4 8)) 36))
	  (subchunk1-size (bytes-to-integer (subseq bytes 16 20)))
	  (audio-format (bytes-to-integer (subseq bytes 20 22))) ;; 1=PCM, linear quantization
	  (channels (bytes-to-integer (subseq bytes 22 24))) ;; number of channels
	  (sample-rate (bytes-to-integer (subseq bytes 24 28))) ;; Sample rate in Hz
	  (byte-rate (bytes-to-integer (subseq bytes 28 32))) ;; == SampleRate * NumChannels * BitsPerSample/8
	  (block-align (bytes-to-integer (subseq bytes 32 34))) ;;BlockAlign       == NumChannels * BitsPerSample/8 The number of bytes for one sample including all channels.
	  (bits-per-sample (bytes-to-integer (subseq bytes 34 36))) ;; 8 bits = 8, 16 bits = 16, etc.
	  ;; If it's not PCM there may be more here....  Specifically:.. 2   ExtraParamSize   if PCM, then doesn't exist;  X   ExtraParams      space for extra parameters
	  (data-size (bytes-to-integer (subseq bytes 40 44)))
	  (data (subseq bytes 44)))
      (declare (ignore subchunk2-size data-size))
      (when verbose
	(format t "~20A: ~10d~%" "Audio format" audio-format)
	(format t "~20A: ~10d~%" "Channels" channels)
	(format t "~20A: ~10d~%" "Sample rate" sample-rate)
	(format t "~20A: ~10d~%" "Byte rate" byte-rate)
	(format t "~20A: ~10d~%" "Bits per sample" bits-per-sample)
	(force-output t))
      (assert (= subchunk1-size 16)) ;; 16 for PCM...
      (assert (= audio-format 1))    ;; o/w it's compressed....
      (assert (= byte-rate (* sample-rate channels (/ bits-per-sample 8))))
      (assert (= block-align (* channels (/ bits-per-sample 8))))
      ;; Beginning of SubChunk2...
      (assert (equal (map 'string 'code-char (subseq bytes 36 40)) "data"))
      ;; If it's not 8-bit data, make the array into a *sample* array, not a *byte* array
      (setf data (byte-array-to-int-array data bits-per-sample))
      (let* ((divisor (expt 2 (1- bits-per-sample))))
	(setf data (map 'vector (lambda (x) (coerce (/ x divisor) 'single-float)) data))
	(make-audio-segment :seconds (float (/ (length data) byte-rate))
			    :sample-rate (coerce sample-rate 'double-float)
			    :source (format nil "Read from Wav file: ~A" filename)
			    :sample-resolution bits-per-sample
			    :samples data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Writing WAV files...  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Writing WAV files...")

;; Do I need to make the int array consist of unsigned ints?!?!?!

(defun save-wav-file (filename samples &key (verbose t) (sample-rate 12800))
  "Read a wav audio file. See http://www.sonicspot.com/guide/wavefiles.html.  Or: https://ccrma.stanford.edu/courses/422/projects/WaveFormat/"
  (when verbose (format t "Saving WAV file: ~A~%" filename))
  ;; Make sure all the sample values are between -1.0 and 1.0
  (when (not (every (lambda (x) (and (< x 1.0) (> x -1.0))) samples))
    (let ((max-abs (reduce 'max-abs samples)))
      (setf samples (map 'vector (lambda (x) (/ x max-abs)) samples))))

  (with-open-file (f filename :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (let* ((channel-count 1)
	   (sample-count (length samples))
	   (bits-per-sample 16)
	   (subchunk1-size 16) ;; 16 for PCM
	   ;; NumSamples * NumChannels * BitsPerSample/8
	   (subchunk2-size (* sample-count channel-count (/ bits-per-sample 8)))
	   (audio-format 1)  ;; 1=PCM, linear quantization
	   (byte-rate (* sample-rate channel-count (/ bits-per-sample 8)))        ;;                 == SampleRate * NumChannels * BitsPerSample/8
	   (block-align (* channel-count (/ bits-per-sample 8)))                  ;;BlockAlign       == NumChannels * BitsPerSample/8 The number of bytes for one sample including all channels.	   
	   (sample-multiplier (expt 2 (1- bits-per-sample))) ;; so this should be like 256 if it's 8 bits per sample... 65k if 16 bits...  SO ALL THE SAMPLES SHOULD BE LESS THAN 1!!
	   (bytes nil))
      (map nil (lambda (x) (write-byte (char-code x) f)) "RIFF")
      (write-sequence (integer-to-bytes (+ subchunk2-size 36)) f)
      (map nil (lambda (x) (write-byte (char-code x) f)) "WAVE")
      (map nil (lambda (x) (write-byte (char-code x) f)) "fmt ")
      (write-sequence (integer-to-bytes subchunk1-size) f)
      ;; Audio format... 1 = PCM
      (write-sequence (integer-to-bytes audio-format :byte-count 2) f)
      ;; Num of channels (1= mono, etc.
      (write-sequence (integer-to-bytes channel-count :byte-count 2) f)
      ;; Sample rate
      (write-sequence (integer-to-bytes sample-rate :byte-count 4) f)
      ;; Byte rate
      (write-sequence (integer-to-bytes byte-rate :byte-count 4) f)
      ;; Block align
      (write-sequence (integer-to-bytes block-align :byte-count 2) f)
      ;; Bits per sample
      (write-sequence (integer-to-bytes bits-per-sample :byte-count 2) f)      
      ;; Beginning of the data subchunk
      (map nil (lambda (x) (write-byte (char-code x) f)) "data")
      (write-sequence (integer-to-bytes subchunk2-size :byte-count 4) f)
      ;; Now turn the floating point samples into integers?!!?
      (setf samples (map 'vector (lambda (x) (round (* x sample-multiplier))) samples))
      ;; ***Do I need to make the int array consist of unsigned ints?!?!?! ... probably?****
      ;; Convert the ints to bytes
      (setf bytes (int-array-to-byte-array samples bits-per-sample))
      ;; Finally write the samples to the file
      (write-sequence bytes f)
      filename)))


