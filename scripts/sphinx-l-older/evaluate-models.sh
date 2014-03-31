#!/bin/sh

cd /home/belamber/Workspace/cl-asr

test_data_folder=./data/an4/wav/
test_ctl=./data/an4/etc/an4_test.fileids

model_folder=./models/an4-phoneme-models
dictionary=./data/an4/etc/an4.dic
lm=./lm/an4.trigramlm
language_weight=4
word_insertion_penalty=200.0

echo $training_data_folder
echo $model_folder
echo $dictionary
echo $lm
echo $language_weight
echo $word_insertion_penalty

eval_string="(progn (asdf:load-system 'cl-asr)  \
(evaluate-on-test-set \"$test_data_folder\" \"$test_ctl\" t :model-directory \"$model_folder\" :grammar-filename nil :phone-or-word :phone :relative-threshold nil :dictionary \"$dictionary\" :word-insertion-penalty $word_insertion_penalty :language-model \"$lm\" :language-weight $language_weight) \
(quit))"

echo $eval_string

sbcl --noinform --control-stack-size 16 --disable-debugger --eval "$eval_string"

#sbcl --control-stack-size 16 --eval "(progn (asdf:load-system 'cl-asr)  \