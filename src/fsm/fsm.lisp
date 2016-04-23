;;;; Author: Ben Lambert
;;;; ben@benjaminlambert

(in-package :sphinx-l)

;;;; Representation of a finite state machine... (for finite state grammars?)

(defstruct fsm-node
  "A node in a finite state machine for recognition."
  id
  lang-hmm-id)

(defstruct fsm-edge
  "An edge in a FSM recognizer."
  id
  source-id
  destination-id
  label
  lang-hmm-start-id
  lang-hmm-end-id)

(defstruct fsm
  "Finite state machine."
  state-count
  start-state
  terminal-states
  node-list
  edge-list
  edge-count
  vocab)

(defun read-finite-state-grammar (filename)
  "Read a finite state machine specification from a file into a structure."
  (let ((state-count nil)
	(start-state nil)
	(terminal-states nil)
	(node-list '())
	(edge-list '())
	(edge-count 0)
	(vocab '()))
    (do-lines (line filename)
      (let ((tokens (cl-ppcre:split " +" line)))
	(cond ((= (length tokens) 0)
	       nil)
	      ((string-equal (first tokens) "N_States:")
	       (setf state-count (parse-integer (second tokens)))
	       (dotimes (i state-count)
		 (push (make-fsm-node :id i :lang-hmm-id i) node-list)))
	      ((string-equal (first tokens) "Start_State:")
	       (setf start-state (parse-integer (second tokens))))
	      ((string-equal (first tokens) "Terminal_States:")
	       (setf terminal-states (mapcar 'parse-integer (rest tokens))))
	      ((string-equal (first tokens) "Edge")
	       (incf edge-count)
	       (let ((edge (make-fsm-edge :id edge-count
					  :source-id (parse-integer (second tokens))
					  :destination-id (parse-integer (third tokens)))))
		 (when (> (length tokens) 3)
		   (let ((label (remove #\" (fourth tokens))))
		     (setf (fsm-edge-label edge) label)
		     (setf vocab (nunion (list label) vocab :test 'string-equal))))
		 (push edge edge-list)))
	      (t (error "Error parsing FSM grammar.")))))
    ;; We've got everything we need, create the FSM.
    (make-fsm :state-count state-count
	      :start-state start-state
	      :terminal-states terminal-states
	      :edge-list (nreverse edge-list)
	      :node-list (nreverse node-list)
	      :edge-count edge-count
	      :vocab vocab)))

(defun load-grammar (filename &key (model-source :phone) silence-penalty word-insertion-penalty)
  "Load a FSM grammar from a file."
  (let ((fsm (read-finite-state-grammar filename)))
    (create-language-hmm-from-fsm fsm *acoustic-model*  :model-source model-source :silence-penalty silence-penalty :insertion-penalty word-insertion-penalty)))
