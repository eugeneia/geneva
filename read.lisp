;;;; Read MK10 documents.

(in-package :mk10.serialize)

(defun read-mk10 (&optional (input *standard-input*))
  "Parse MK10 document from INPUT."
  (run (mk10.reader::=document) input))
