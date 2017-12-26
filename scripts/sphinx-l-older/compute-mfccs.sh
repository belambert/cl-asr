#!/bin/sh

if [ $# -lt 3 ] ; then
      echo "Usage: compute-mfccs-new.sh <audio directory> <ctl file> <transcript file>"
      exit 0
fi

#cd /home/belamber/Workspace/cl-asr

audio_dir=$1
ctl_file=$2
transcript_file=$3

#audio_dir=./data/audio/aurora/sample/
#audio_dir=./data/audio/aurora/train/
#audio_dir=./data/audio/aurora/test/

sbcl --disable-debugger --eval "(progn (asdf:load-system 'cl-asr) (convert-folder-to-mfcc \"$audio_dir\" :ctl-file \"$ctl_file\" :transcript-file \"$transcript_file\"  ) (quit))"
