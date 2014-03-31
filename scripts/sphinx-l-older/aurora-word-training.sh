#!/bin/sh

cd /home/belamber/Workspace/cl-asr


training_data_folder=./data/audio/aurora/train/
test_data_folder=./data/audio/aurora/test/
#initial_models=./models/iso-digit-models
initial_models=./models/connected-word-models
model_folder=./models/aurora-word-models-6
iterations=6

echo $training_data_folder
echo $model_folder
echo $iterations

eval_string="(progn (asdf:load-system 'cl-asr)  \
(train-connected-word-models \"$training_data_folder\" \"$initial_models\" \"$model_folder\" :iterations  $iterations) \
(evaluate-on-test-set \"$test_data_folder\" nil t :model-directory \"$model_folder\" :grammar-filename \"./grammars/digits.fsm\" :relative-threshold nil)(quit))"

sbcl --control-stack-size 16 --noinform --disable-debugger --eval "$eval_string"

