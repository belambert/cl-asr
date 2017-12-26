#!/bin/sh

cd /home/belamber/Workspace/cl-asr

data_folder=./data/audio/aurora/test/
#data_folder=./data/audio/aurora/sample/
#data_folder=./data/audio/aurora/train/

#model_folder=./models/iso-digit-models
model_folder=./models/ti-train-10


echo $data_folder
echo $model_folder

sbcl --disable-debugger --eval "(progn (asdf:load-system 'cl-asr) (dotimes (i 50) (format t \"Threshold: ~A\" (* i 10))(evaluate-on-test-set \"$data_folder\" nil :model-directory \"$model_folder\" :grammar-filename \"./grammars/digits.fsm\" :relative-threshold (* i 10))) (quit))" 
