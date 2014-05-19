;;;; Shared utility functions used by various components of Geneva.

(defpackage geneva.utilities
  (:documentation
   "Shared utility functions used by various components of Geneva.")
  (:use :geneva
        :cl)
  (:export :*default-title*
           :*default-index-caption*
           :*index-p*
           :*index-headers-p*
           :null-level
           :descend-level
           :incf-level
           :level-string
           :text-string
           :document-index))

(in-package :geneva.utilities)

(defparameter *default-title* "Untitled"
  "Default title for documents.")

(defparameter *default-index-caption* "Table of Contents"
  "Default caption for indexes.")

(defparameter *index-p* t
  "Controls wether an index is rendered.")

(defparameter *index-headers-p* t
  "Controls wether headers are numbered.")

(defun null-level ()
  "Returns the root level."
  (cons 1 nil))

(defun descend-level (level)
  "Returns the next deeper LEVEL."
  (append level (null-level)))

(defun incf-level (level)
  "Increment LEVEL by one."
  (incf (elt level (length (rest level)))))

(defun level-string (level)
  "Return string representation for LEVEL."
  (format nil "~{~a~^.~}" level))

(defun text-string (text)
  "Return TEXT string without markup."
  (with-output-to-string (*standard-output*)
    (dolist (text-part text)
      (write-string (if (stringp text-part)
                        text-part
                        (content-values text-part))))))

(defun document-index-2 (document level)
  "Base function for DOCUMENT-INDEX."
  (flet ((section-p (content)
           (eq (content-type content) +section+))
         (section-entry (section)
           (prog1 (multiple-value-bind (header contents)
                      (content-values section)
                    (list (copy-list level)
                          header
                          (document-index-2
                           contents (descend-level level))))
             (incf-level level))))
    (mapcar #'section-entry (remove-if-not #'section-p document))))

(defun document-index (document)
  "Returns section hierarchy on DOCUMENT."
  (document-index-2 document (null-level)))
