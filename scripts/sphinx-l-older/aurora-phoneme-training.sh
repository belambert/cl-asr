#!/bin/sh

cd /home/belamber/Workspace/cl-asr

training_data_folder=./data/audio/aurora/train/
test_data_folder=./data/audio/aurora/test/
initial_models=./models/aurora-initial-phoneme-models
model_folder=./models/aurora-phoneme-models-train-10
iterations=10
dictionary=./dict/numbers.dict

echo $training_data_folder
echo $model_folder
echo $iterations

eval_string="(progn (asdf:load-system 'cl-asr)  \
(train-phoneme-models-user \"$training_data_folder\" \"$initial_models\" \"$model_folder\"  $iterations \"$dictionary\"  :save-intermediate-models nil  :evaluate-intermediate-models nil :evaluation-data-folder \"$test_data_folder\") \
(evaluate-on-test-set \"$test_data_folder\" nil t :model-directory \"$model_folder\" :grammar-filename \"./grammars/digits.fsm\" :phone-or-word :phone :relative-threshold nil :dictionary \"$dictionary\")(quit))"

sbcl --control-stack-size 16 --disable-debugger --eval "$eval_string"
