;;;; Print MK10 documents.

(defpackage mk10.printer
  (:use :cl
	:mk10
        :mk10.tokens
        :pretty-string
        :split-sequence))

(in-package :mk10.printer)

(defparameter *columns* 72
  "Maximum line width.")

(defparameter *table-padding* 1
  "Minimum number of spaces between table columns.")

(defvar *indent* 0
  "Indent level.")

(defparameter *indent-max* (/ *columns* 4)
  "Indent limit.")

(defparameter *to-escape* (append *special-tokens* *markup-directives*)
  "Tokens to escape.")

(defparameter *beginning* t
  "Flag indicating wether this is the bedinning of the document.")

(defun escape (string)
  "Escape special tokens STRING."
  (flet ((needs-escape-p (char) (member char *to-escape*))
	 (escape-char (char) (format nil "\\~c" char)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
	 for pos = (position-if #'needs-escape-p string :start start)
	 do (write-sequence string out :start start :end pos)
	 when pos do (write-sequence (escape-char (char string pos)) out)
	 while pos))))

(defun print-spaces (n)
  "Print N spaces."
  (dotimes (x n)
    (write-char #\Space)))

(defun print-string (string &key (wrap t))
  "Print STRING indented and optionally WRAP it."
  (let ((lines (split-sequence
                #\Newline
                (if wrap
                    (wrap-string string (- *columns* *indent*))
                    string))))
    (loop for line in (if wrap (butlast lines) lines)
       do
         (print-spaces *indent*)
         (write-string line)
         (terpri))))

(defun markup-string (markup)
  "Return string for MARKUP."
  (flet ((escape-and-decorate
             (delimiter &optional (delimiter2 delimiter))
           (format nil "~a~a~a"
                   delimiter
                   (escape (content-values markup))
                   delimiter2)))
    (ecase (content-type markup)
      (#.+bold+    (escape-and-decorate #\*))
      (#.+italic+  (escape-and-decorate #\_))
      (#.+code+    (escape-and-decorate #\{ #\}))
      (#.+url+     (escape-and-decorate #\[ #\])))))

(defun text-string (text)
  "Return string for TEXT."
  (with-output-to-string (*standard-output*)
    (dolist (item text)
      (write-string (if (stringp item)
                        (escape item)
                        (markup-string item))))))

(defun listing-string (items)
  "Return lsting string for ITEMS."
  (with-output-to-string (*standard-output*)
    (dolist (item items)
      (format t "+ ~a~%" (text-string item)))))

(defun caption-string (type caption)
  "Return caption string for TYPE and CAPTION."
  (format nil "#~a ~a#" type (text-string caption)))

(defun table-string (rows)
  "Return string for ROWS."
  (flet ((widths (rows)
           (let ((widths nil))
             (dotimes (x (length (first rows)) (reverse widths))
               (push (loop for row in rows
                        maximize (length (nth x row)))
                     widths))))
         (items-to-strings (rows)
           (loop for row in rows
              collect (loop for item in row
                         collect (text-string item)))))
    (let* ((string-table (items-to-strings rows))
           (table-widths (widths string-table)))
      (with-output-to-string (*standard-output*)
        (dolist (row string-table)
	  (loop for item in row
	        for width in table-widths
	     do
               (format t "| ~a" item)
	       (print-spaces (- (+ width *table-padding*)
				(length item))))
	  (terpri))))))

(defun print-content (content)
  "Print CONTENT."
  (ecase (content-type content)
    
    (#.+paragraph+ (print-string
                    (text-string (content-values content)))
                   (terpri))

    (#.+listing+   (print-string
                    (listing-string (content-values content))))

    (#.+table+     (multiple-value-bind (caption rows)
                       (content-values content)
                     (print-string (caption-string "table" caption))
                     (print-string (table-string rows) :wrap nil)))

    (#.+media+     (multiple-value-bind (caption url)
                       (content-values content)
                     (print-string (caption-string "media" caption))
                     (print-string url :wrap nil)
                     (terpri)))

    (#.+pre+       (multiple-value-bind (caption pre)
                       (content-values content)
                     (print-string (caption-string "code" caption))
                     (print-string pre :wrap nil)
                     (print-string "#" :wrap nil)
                     (terpri)))

    (#.+section+   (multiple-value-bind (header contents)
                       (content-values content)
                     ;; Delimit following sections with extra whitespace
                     (if *beginning*
                         (setf *beginning* nil)
                         (terpri))
                     (print-string
                      (format nil "< ~a" (text-string header)))
                     (terpri)
                     (incf *indent*)
                     (unwind-protect
                          (dolist (content contents)
                            (print-content content))
                       (decf *indent*))
                     (print-string ">" :wrap nil)
                     (terpri)))))

;;; Interface to MK10.PRINTER.

(in-package :mk10)

(defun print-mk10 (document &optional (stream *standard-output*)
                            &key (columns mk10.printer::*columns*))
  "Print DOCUMENT to STREAM optimized for COLUMNS."
  (let ((mk10.printer::*columns* columns)
        (*standard-output* stream)
        (mk10.printer::*indent* 0)
        (mk10.printer::*beginning* t))
    (dolist (content document)
      (mk10.printer::print-content content))))
