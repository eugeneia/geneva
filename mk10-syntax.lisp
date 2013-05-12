;;;; Defines named readtable for text markup syntax.

(in-package :mk10.macros)

(defun make-markup-reader (constructor)
  "Returns function that reads string literal and applies CONSTRUCTOR."
  (lambda (stream subchar arg)
    `(funcall ,constructor ,(read stream t))))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\b (make-markup-reader #'make-bold))
  (:dispatch-macro-char #\# #\i (make-markup-reader #'make-italic))
  (:dispatch-macro-char #\# #\c (make-markup-reader #'make-code))
  (:dispatch-macro-char #\# #\u (make-markup-reader #'make-url))
  (:case :invert))
