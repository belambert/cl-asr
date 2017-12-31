#!/bin/sh

sbcl --dynamic-space-size 4000 --noinform --noprint --non-interactive --eval "(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (asdf:load-system :cl-asr))" --eval "(cl-asr::sphinxlisp-decode)" $@
