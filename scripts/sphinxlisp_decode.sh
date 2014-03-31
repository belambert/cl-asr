#!/bin/sh


#sbcl --dynamic-space-size 4000 --noinform --noprint --non-interactive --eval "(asdf:load-system :sphinx-l)" --eval "(sphinx-l::sphinxlisp-decode)" $@
#sbcl --dynamic-space-size 8000 --noinform --noprint --non-interactive --eval "(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (asdf:load-system :sphinx-l))" --eval "(sphinx-l::sphinxlisp-decode)" $@

~/software/bin/sbcl --dynamic-space-size 4000 --noinform --noprint --non-interactive --eval "(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (asdf:load-system :sphinx-l))" --eval "(sphinx-l::sphinxlisp-decode)" $@

#~/software/bin/sbcl --dynamic-space-size 8000 --noinform --noprint --non-interactive --userinit "/afs/cs.cmu.edu/user/belamber/.sbclrc" --eval "(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (asdf:load-system :sphinx-l))" --eval "(sphinx-l::sphinxlisp-decode)" $@



# This was from messing around, trying to get this to run on the cluster!

#~/software/bin/sbcl --dynamic-space-size 8000 --noinform --noprint --non-interactive --userinit "/net/katsura/work/belamber/hg/blambert-util/etc/sbclrc" --eval "(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (asdf:load-system :sphinx-l))" --eval "(sphinx-l::sphinxlisp-decode)" $@
#~/.quicklisp/dists/quicklisp/software/split-sequence-20101006-http/
#~/software/bin/sbcl --dynamic-space-size 8000 --userinit "/net/katsura/work/belamber/hg/blambert-util/etc/sbclrc" \
#--eval "(pprint ( ASDF:USER-SOURCE-REGISTRY-DIRECTORY))" \
#--eval "(push \"/net/katsura/work/belamber/hg/sphinx-l/\" asdf:*central-registry*)" \
#--eval "(push \"/net/katsura/work/belamber/hg/blambert-util/\" asdf:*central-registry*)" \
#--eval "(push \"/net/katsura/work/belamber/hg/kbasr4/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/bordeaux-fft-20101006-http/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/cl-ppcre-2.0.3/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/ieee-floats-20101006-darcs/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/alexandria-20111203-git/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/metatilities-base-20111105-git/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/array-operations-20101006-git/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/cl-fad-0.6.4/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/split-sequence-20101006-http/\" asdf:*central-registry*)" \
#--eval "(let ((*standard-output* (make-broadcast-stream))(*error-output* (make-broadcast-stream))) (asdf:load-system :sphinx-l))" --eval "(sphinx-l::sphinxlisp-decode)" $@
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/\" asdf:*central-registry*)" \
#--eval "(push \"/afs/cs.cmu.edu/user/belamber/.quicklisp/dists/quicklisp/software/\" asdf:*central-registry*)" \
