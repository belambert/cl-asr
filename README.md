Lisp ASR decoder
================
[![Build Status](https://travis-ci.org/belambert/cl-asr.svg?branch=master)](https://travis-ci.org/belambert/cl-asr)

ASR decoder written in Common Lisp.  Uses acoustic models trained by Sphinx, and
ARPA LM files.

This may not currently be fully functional, but it's probably close.

This can be run in batch mode on Sphinx feature extracted files.  The
'standalone' component operates on raw WAV files and does the feature
extraction internally.


Command line usage
------------------

Run from the command line args similar to Sphinx3 decode:

    scripts/decode.sh

API usage
---------

Use this function to Load a Sphinx3 model:
```lisp
(defun load-s3-model (&key folder mdef mean var mixw tmat lda dict))
```

You can either specify a folder containing all the model file (with standard
names) or specify each model file individually.

Here's an example of loading models from a folder:
```lisp
(defvar *am* (cl-asr::load-s3-model :folder "/Users/bel/workspace/sphinx/acoustic-models/fisher/"))
```

The language model gets loaded like so:
```lisp
(defvar *lm* (cl-lm::load-model <filename>))
```
