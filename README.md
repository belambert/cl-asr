============
Sphinx-L ASR decoder
============


ASR decoder.

Uses acoustic models trained by Sphinx.

Can be run in batch mode on SPhinx feature extracted files.

The 'standalone' component, operates on raw WAV files and does the feature extraction internally.


Command line usage
------------------

Run from the command line args similar to Sphinx3 decode:
scripts/sphinxlisp_decode.sh


API usage
---------

To Load a Sphinx3 model:
(defun load-s3-model (&key folder mdef mean var mixw tmat lda dict))
Example:
(sphinx-l::load-s3-model :folder "/Users/bel/workspace/sphinx/acoustic-models/fisher/")

The lanugage model gets loaded like so:
(setf *lm* (language-model::load-model (gethash "lm" opts)))


(TRAIN-MODEL filename)
is the main function for training models

...

Notes
-----

 * ...




