;;; Copyright Benjamin E. Lambert, 2005-2011
;;; All rights reserved
;;; Please contact author regarding licensing and use:
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :sphinx-l)
(cl-user::file-summary "Plot language HMMs as DOT directed graphs")


(defun language-hmm->dot-file (hmm dot)
  "This BIG function takes a langauge HMM structure and prints the structure, etc. as a GraphViz DOT file.
   This function is intended to be used for debugging the language HMM construction, to make sure all the
   connections are being created as expected.  This should almost certainly never be used on anything more
   than a toy vocabulary with a handful of words."
  (with-open-file (f dot :direction :output :if-exists :supersede)
    (format f "digraph language_hmm {~%")
    (format f "rankdir=LR;~%")
    ;;(format f "ranksep=.75;~%")
    ;;(format f "ordering=out;~%")
    ;;(format f "minlen=0;~%")
    (format f "concentrate=true;~%")
    ;; First, print all the edges in the graph, and when their weights are non-zero, label
    ;; the edge with the probability.
    (loop for i from 0 below (language-hmm-state-count hmm)
	 for transitions = (aref (language-hmm-transitions hmm) i) 
	 for transition-probabilities = (aref (language-hmm-transition-probabilities hmm) i) do
	 (loop for transition in transitions 
	    for probability in transition-probabilities do
	      (format f "~D -> ~D" i transition)
	      (when (not (zerop probability))
		(setf probability (expt (language-hmm-log-base hmm) probability))
		(format f " [label=\"~,2f\"]" probability))
	      (when (or (= i 0) (= transition 3))
	      	(format f " [weight=8]"))
	      (format f ";~%")))
    (loop for i from 0 below (language-hmm-state-count hmm)
       for this-word = (aref (language-hmm-word-id hmm) i)
       for this-phone-id = (aref (language-hmm-phone-id hmm) i)
       for this-phone = (gethash this-phone-id (language-hmm-phone-id-table hmm)) do
	 (cond ((= i (language-hmm-start-state hmm))
		(format f "subgraph {~%rank = source; ~%")
		(format f "~D [peripheries=3];~%" i)
		(format f "};~%"))
	       
	       ((find i (language-hmm-final-states hmm))
		(format f "subgraph {~%rank = sink; ~%")
		(format f "~D [peripheries=5];~%" i)
		(format f "};~%"))
	       ((= i 1)
		(format f "subgraph {~%rank = min;~%")
		(format f "~D;~%" i)
		(format f "};~%"))
	       ((= i 2)
		(format f "subgraph {~%rank = max; ~%")
		(format f "~D;~%" i)
		(format f "};~%"))
	       (t  ;; Otherwise!
		
		;; Always wrap a state in a subgraph... dot appears to group the subgraphs, even if they aren't contiguous...
		;; as long as they have a common name.
		(when this-word
		  (format f "subgraph \"cluster_~A\" {~%label = \"~A\"; fontsize=40; fontcolor=red;~%" this-word this-word))
		(when this-phone
		  (format f "subgraph \"cluster_~A\" {~%label = \"~A\"; fontsize=12; fontcolor=blue;~%" this-phone-id (make-triphone-readable this-phone)))
		
		;; this is where we print the NODE
		(format f "~D [" i)
		  ;; Emitting states are filled...
		(when (aref (language-hmm-emission-distributions hmm) i)
		  (format f "style=filled,"))
		;; Word final states are blue, and have a special label that includes the word
		(when (aref (language-hmm-word-final-state hmm) i)
		  (format f "color=lightblue,style=filled,")
		  (format f "label=\"~D: ~A\"," i (aref (language-hmm-hmm-state-words hmm) i)))	   
		(format f "];~%")
		;; DONE PRINTING THE NODE
		(when this-word (format f "};~%"))
		(when this-phone (format f "};~%"))
		)))
  (format f "}~%")))

