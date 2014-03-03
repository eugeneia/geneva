;;;; Read mk2 documents.

(in-package :geneva.mk2)

(defun read-mk2 (&optional (input *standard-input*))
  "Read MK2 document from INPUT."
  (make-document (run (=document) input)))
