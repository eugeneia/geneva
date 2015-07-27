;;;; Shared utility functions used by various components of Geneva.

(defpackage geneva.utilities
  (:documentation
   "Shared utility functions used by various components of Geneva.")
  (:use :geneva
        :cl
        :split-sequence)
  (:export :*default-index-caption*
           :*index-p*
           :*index-headers-p*
           :null-level
           :descend-level
           :incf-level
           :level-string
           :text-string
           :document-index
           :wrap-string
           :align-string))

(in-package :geneva.utilities)

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
    (dolist (text-token text)
      (write-string (content-values text-token)))))

(defun document-index-2 (document level)
  "Base function for DOCUMENT-INDEX."
  (flet ((section-p (content)
           (eq (content-type content) :section))
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

(defun wrap-string (string &optional (columns 72))
  "Return copy of STRING with spaces replaced by newlines so that lines
do not exceed COLUMNS characters when possible. COLUMNS defaults to 72."
  (with-output-to-string (out)
    (loop for line in (split-sequence #\Newline string)
       do
         (loop for word in (split-sequence #\Space line)
            for word-length = (length word)
            with count = 0
            do (cond ((> (+ count word-length) (1+ columns))
                      (fresh-line out)
                      (write-string word out)
                      (setf count (1+ word-length)))
                     (t
                      (unless (= count 0)
                        ;; No Space at beginning of line
                        (write-char #\Space out))
                      (write-string word out)
                      (incf count (1+ word-length)))))
         (terpri out))))

(defun align-string (string alignment &optional (columns 72))
  "Return aligned copy of STRING with respect to COLUMNS. Possible values
for ALIGNMENT are :RIGHT and :CENTER."
  (with-output-to-string (*standard-output*)
    (loop for line
          in (split-sequence #\Newline (wrap-string string columns))
       when (not (string-equal "" line))
       do (format t "~a~a~%"
                  (make-string
                   (ecase alignment
                     (:right (1+ (- columns (length line))))
                     (:center (ceiling (- columns (length line)) 2)))
                   :initial-element #\Space)
                  line))))
