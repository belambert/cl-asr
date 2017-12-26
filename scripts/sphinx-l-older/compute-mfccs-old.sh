#!/bin/sh

cd /home/belamber/Workspace/cl-asr

#audio_dir=./data/audio/aurora/sample/
audio_dir=./data/audio/aurora/train/
#audio_dir=./data/audio/aurora/test/

sbcl --disable-debugger --eval "(progn (asdf:load-system 'cl-asr) (convert-folder-to-mfcc-simple \"$audio_dir\" :filename-transcript-function 'ti-digit-filename->transcript) (quit))"
