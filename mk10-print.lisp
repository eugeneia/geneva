;;;; Export document as plain text.

(defpackage document.export.plain
  (:use :cl
	:document)
  (:export :*line-width*
	   :*indent-max*
	   :*table-padding*
	   :export-document-plain))

(in-package :document.export.plain)

(defparameter *special-characters* "<>+*_{}[]#\\"
  "Special characters that need to be escaped.")

(defparameter *line-width* 72
  "Maximum line width.")

(defparameter *indent-max* (/ *line-width* 2)
  "Indent limit.")

(defparameter *table-padding* 1
  "Minimum number of spaces between table columns.")

(defvar *line-index* 0
  "Line index.")

(defvar *indent-level* 0
  "Indent level.")

(defun escape (string)
  "Escape special characters in STRING."
  (flet ((needs-escape-p (char) (find char *special-characters*))
	 (escape-char (char) (format nil "\\~c" char)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
	 for pos = (position-if #'needs-escape-p string :start start)
	 do (write-sequence string out :start start :end pos)
	 when pos do (write-sequence (escape-char (char string pos)) out)
	 while pos))))

(defun text-to-string (text)
  "Return a string of TEXT."
  (with-output-to-string (out)
    (dolist (item text)
      (if (stringp item)
	  (write-string (escape item) out)
	  (format out
		  (case (content-type item)
		    (#.+bold+   "*~a*")
		    (#.+italic+ "_~a_")
		    (#.+code+   "{~a}")
		    (#.+url+    "[~a]")
		    (t (error "Invalid content type in TEXT: ~S."
			      (content-type item))))
		  (escape (content-values item)))))))

(defun new-line ()
  "Print new line."
  (write-char #\newline)
  (setf *line-index* 0))

(defun empty-line ()
  "Print a new line if not at a fresh line already."
  (fresh-line)
  (setf *line-index* 0))

(defun print-spaces (n)
  "Print N spaces."
  (dotimes (x n)
    (write-char #\Space))
  (incf *line-index* n))

(defun indent ()
  "Print indenting white space."
  (when (= *line-index* 0)
    (let ((indent-n (if (< *indent-level* *indent-max*)
			*indent-level*
			*indent-max*)))
      (print-spaces indent-n)
      (setf *line-index* indent-n))))

(defun write-s (string &key (start 0) end)
  "Print STRING from START to END and increment *line-index*."
  (write-string string *standard-output* :start start :end end)
  (incf *line-index* (- (or end (length string)) start)))

(defun print-proper (string)
  "Print STRING spread on lines with length not exceeding *line-width*."
  (let ((length (length string)))
    (labels ((delimeter-p (char)
	       (or (char= #\Space char)
		   (char= #\Tab char)))
	     (line-end (string max start)
	       (if (>= max (- length start))
		   length
		   (let ((width (+ max start)))
		     (loop for end = width then (1- end)
			when (and (delimeter-p (char string end))
				  (not (delimeter-p
					(char string (1- end)))))
			return end
			unless (< start end)
			return
			  (loop for long-end = (+ width 1)
			     then (1+ long-end)
			     when (or (delimeter-p
				       (char string long-end))
				      (= long-end length))
			     return long-end))))))
      (loop for nil = (indent)
	 for start = 0 then (1+ end)
	 for end = (line-end string (- *line-width* *line-index*) start)
	 do
	   (write-s string :start start :end end)
	   (when (or (= *line-width*
			(+ *line-index* (- end start)))
		     (< end length))
	     (new-line))
	 while (> length end)))))

(defun print-text (text)
  "Print TEXT properly."
  (print-proper (text-to-string text)))

(defun print-paragraph (paragraph)
  "Print PARAGRAPH."
  (print-text (content-values paragraph))
  (empty-line)
  (new-line))

(defun print-listing (listing)
  "Print LISTING."
  (let* ((item-prefix "+ ")
         (item-indent-delta (length item-prefix)))
    (dolist (item (content-values listing))
      (indent)
      (write-s item-prefix)
      (incf *indent-level* item-indent-delta)
      (print-text item)
      (decf *indent-level* item-indent-delta)
      (empty-line)))
  (new-line))

(defun print-table (table)
  "Print TABLE."
  (indent)
  (write-s "#table ")
  (multiple-value-bind (description rows)
      (content-values table)
    (print-text description)
    (write-s "#")
    (empty-line)
    (flet ((widths (rows)
	     (let ((widths nil))
	       (dotimes (x (length (first rows)) (reverse widths))
		 (push (loop for row in rows
			  maximize (length (nth x row)))
		       widths))))
	   (items-to-strings (rows)
	     (loop for row in rows
		collect (loop for item in row
			   collect (text-to-string item)))))
      (let* ((string-table (items-to-strings rows))
	     (table-widths (widths string-table)))
	(dolist (row string-table)
	  (indent)
	  (loop
	     for item in row
	     for width in table-widths
	     do
               (write-s "| ")
	       (write-s item)
	       (print-spaces (- (+ width *table-padding*)
				(length item))))
	  (new-line)))))
  (new-line))

(defun print-media (media-object)
  "Print MEDIA-OBJECT."
  (indent)
  (write-s "#media ")
  (multiple-value-bind (description url)
      (content-values media-object)
    (print-text description)
    (write-s "#")
    (empty-line)
    (indent)
    (write-s url))
  (new-line)
  (new-line))

(defun print-pre (string)
  "Print preformatted STRING."
  (let ((lines (loop for character across string
		  for pos = 0 then (1+ pos)
		  when (char= character #\Newline)
		  collect pos)))
    (loop for end in lines
       for start = 0 then (1+ end)
       do
	 (indent)
	 (write-s string :start start :end end)
	 (new-line)
       finally
	 (indent)
	 (write-s string :start (1+ end))
	 (new-line))))

(defun print-code (code-object)
  "Print CODE-OBJECT."
  (indent)
  (write-s "#code ")
  (multiple-value-bind (description code)
      (content-values code-object)
    (print-text description)
    (write-s "#")
    (empty-line)
    (print-pre code))
  (indent)
  (write-s "#")
  (new-line)
  (new-line))

(defun print-section (section)
  "Print SECTION."
  (indent)
  (write-s "< ")
  (multiple-value-bind (description contents)
      (content-values section)
    (print-text description)
    (empty-line)
    (new-line)
    (dolist (content contents)
      (print-content content nil)))
  (indent)
  (write-s ">")
  (empty-line)
  (new-line))

(defun print-content (content &optional (top-level-p t))
  "Print CONTENT."
  (case (content-type content)
    (#.+paragraph+ (print-paragraph content))
    (#.+listing+   (print-listing content))
    (#.+table+     (print-table content))
    (#.+media+     (print-media content))
    (#.+pre+       (print-code content))
    (#.+section+   (if top-level-p
		       (print-section content)
		       (progn (incf *indent-level*)
			      (print-section content)
			      (decf *indent-level*))))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun export-document-plain (document stream)
  "Print DOCUMENT as plain text to STREAM."
  (let ((*standard-output* stream)
        (*line-index* 0)
        (*indent-level* 0))
    (dolist (content document)
      (print-content content))))
