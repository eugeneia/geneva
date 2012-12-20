;;;; Export document as org-mode text.

(defpackage document.export.org-mode
  (:use :cl
	:document)
  (:export :export-document-org-mode))

(in-package :document.export.org-mode)

(defvar *header-level* 0
  "Header level.")

(defun text-to-string (text)
  "Return a string of TEXT."
  (with-output-to-string (out)
    (dolist (item text)
      (if (stringp item)
	  (write-string item out)
	  (format out
		  (case (content-type item)
		    (#.+bold+   "*~a*")
		    (#.+italic+ "/~a/")
		    (#.+code+   "=~a=")
		    (#.+url+    "[[~a]]")
		    (t (error "Invalid content type in TEXT: ~S."
			      (content-type item))))
		  (content-values item))))))

(defun print-paragraph (text)
  "Print {TEXT} as a paragraph."
  (document.export.plain::print-proper
   (text-to-string text))
  (document.export.plain::empty-line)
  (document.export.plain::new-line))

(defun print-code (code-object)
  "Print {CODE}."
  (multiple-value-bind (description code)
      (content-values code-object)
    (format t "#+BEGIN_SRC~%")
    (write-string code)
    (document.export.plain::empty-line)
    (format t "#+END_SRC~%")
    (print-paragraph description)))

(defun print-section (section)
  "Print {SECTION}."
  (multiple-value-bind (header contents)
      (content-values section)
    (dotimes (i *header-level*)
      (declare (ignorable i))
      (write-string "*"))
    (write-string " ")
    (print-paragraph header)
    (dolist (content contents)
      (print-content content))))

(defun print-content (content)
  "Print {CONTENT}."
  (case (content-type content)
    (#.+paragraph+ (print-paragraph (content-values content)))
    (#.+listing+   (document.export.plain::print-listing content))
;    (#.+table+     (print-table content))
;    (#.+media+     (print-media content))
    (#.+pre+       (print-code content))
    (#.+section+   (let ((*header-level* (1+ *header-level*)))
		     (print-section content)))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun export-document-org-mode (document stream)
  "Print {DOCUMENT} as plain text to {STREAM}."
  (let ((*standard-output* stream)
        (document.export.plain::*line-index* 0)
        (document.export.plain::*indent-level* 0)
	(*header-level* 0))
    (dolist (content document)
      (print-content content))))