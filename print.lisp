;;;; Print mk2 documents.

(in-package :geneva.mk2)

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
      (#.+bold+        (escape-and-decorate #\*))
      (#.+italic+      (escape-and-decorate #\_))
      (#.+fixed-width+ (escape-and-decorate #\{ #\}))
      (#.+url+         (escape-and-decorate #\[ #\])))))

(defun text-string (text)
  "Return string for TEXT."
  (with-output-to-string (*standard-output*)
    (dolist (item text)
      (write-string (if (stringp item)
                        (escape item)
                        (markup-string item))))))

(defun listing-string (items &optional (bullet "+ "))
  "Return listing string for ITEMS using BULLET."
  (with-output-to-string (*standard-output*)
    (dolist (item items)
      (format t "~a~a"
              bullet
              (let* ((*indent* (length bullet))
                     (*columns* (- *columns* *indent*)))
                (subseq
                 (with-output-to-string (*standard-output*)
                   (print-string (text-string item)))
                 *indent*))))))

(defun caption-string (type-string caption)
  "Return caption string for TYPE-STRING and CAPTION."
  (format nil "#~a~@[ ~a~]#"
          (string-downcase type-string)
          (when caption (text-string caption))))

(defun table-string (rows &optional (delimiter "| "))
  "Return string for ROWS using DELIMITER."
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
               (format t "~a~a" delimiter item)
	       (print-spaces (- (+ width *table-padding*)
				(length item))))
	  (terpri))))))

(defun print-plaintext (string)
  "Print STRING as plaintext body (with escaped
<object-deilimiter> <element-delimiter> sequences)."
  (let ((terminator (format nil "~%#~%~%"))
        (escaped (format nil "~%\\#~%~%")))
    (loop for start = 0 then (+ pos (length terminator))
          for pos = (search terminator string :start1 1 :start2 start)
                    then (search terminator string :start2 start)
       do (write-string string *standard-output* :start start :end pos)
       when pos do
         (case pos
           (0 (write-string escaped *standard-output* :start 1))
           (otherwise (write-string escaped)))
       while pos)))

(defun print-content (content)
  "Print CONTENT."
  (ecase (content-type content)
    
    (#.+paragraph+ (print-string
                    (text-string (content-values content)))
                   (terpri))

    (#.+listing+   (print-string
                    (listing-string (content-values content))
                    :wrap nil))

    (#.+table+     (multiple-value-bind (caption rows)
                       (content-values content)
                     (print-string
                      (caption-string *table-keyword* caption))
                     (print-string (table-string rows) :wrap nil)))

    (#.+media+     (multiple-value-bind (caption url)
                       (content-values content)
                     (print-string
                      (caption-string *media-keyword* caption))
                     (print-string url :wrap nil)
                     (terpri)))

    (#.+plaintext+ (multiple-value-bind (caption pre)
                       (content-values content)
                     (print-string
                      (caption-string *plaintext-keyword* caption))
                     (print-plaintext pre)
                     (format t "~&")
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

(defun print-mk2 (document &optional (stream *standard-output*)
                           &key      (columns *columns*))
  "Print DOCUMENT to STREAM optimized for COLUMNS."
  (let ((*columns* columns)
        (*standard-output* stream)
        (*indent* 0)
        (*beginning* t))
    (dolist (content document)
      (print-content content))))
