#!/bin/sh

cd /home/belamber/Workspace/cl-asr

training_data_folder=./data/an4/wav/
train_ctl=./data/an4/etc/an4_train.fileids

test_data_folder=./data/an4/wav/
test_ctl=./data/an4/etc/an4_test.fileids

initial_models=./models/an4/initial-models/
model_folder=./models/an4/phoneme-2-iter
iterations=2
#dictionary=./data/an4/etc/an4.dic
dictionary=./dict/cmudict.0.7a.txt

#some other initial models we could try...
#an4-initial-models/
#phone-models-from-digits-short-1-iter/
#phone-models-from-digits-short-2-iter/
#phone-models-from-digits-short-3-iter/
#phone-models-from-digits-short

echo $training_data_folder
echo $model_folder
echo $iterations

eval_string="(progn (asdf:load-system 'cl-asr)  \
(train-phoneme-models-user \"$training_data_folder\" \"$initial_models\" \"$model_folder\"  $iterations \"$dictionary\"  :save-intermediate-models nil :evaluate-intermediate-models nil :evaluation-data-folder \"$test_data_folder\" :ctl-file \"$train_ctl\" ) \
(evaluate-on-test-set \"$test_data_folder\" \"$test_ctl\" t :model-directory \"$model_folder\" :grammar-filename \"./grammars/an4.fsm\" :phone-or-word :phone :relative-threshold nil :dictionary \"$dictionary\") \
(quit))"

echo $eval_string

sbcl --control-stack-size 16 --disable-debugger --eval "$eval_string"

#sbcl --control-stack-size 16 --eval "(progn (asdf:load-system 'cl-asr)  \
