Lisp ASR decoder
================
ASR decoder written in Common Lisp.  Uses acoustic models trained by Sphinx.

Can be run in batch mode on Sphinx feature extracted files.  The 'standalone' component operates on raw WAV files and does the feature extraction internally.


Command line usage
------------------

Run from the command line args similar to Sphinx3 decode:
```bash
scripts/sphinxlisp_decode.sh
```

API usage
---------

To Load a Sphinx3 model:
```lisp
(defun load-s3-model (&key folder mdef mean var mixw tmat lda dict))
```

Example:
```lisp
(cl-asr::load-s3-model :folder "/Users/bel/workspace/sphinx/acoustic-models/fisher/")
```

The lanugage model gets loaded like so:
```lisp
(setf *lm* (cl-lm::load-model (gethash "lm" opts)))
```

The main function for training models
```lisp
(TRAIN-MODEL filename)
```
