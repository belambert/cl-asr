#!/bin/sh


sbcl --dynamic-space-size 1000 --noinform --end-runtime-options --disable-debugger --noprint --eval "(asdf:load-system 'cl-asr :verbose nil)" --eval "(cl-asr::sphinx3-decode \"\")" --eval "(quit)" $@ 

