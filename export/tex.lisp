;;;; *foo bar* -> {\bf foo bar\/}
;;;; _foo bar_ -> {\it foo bar\/}
;;;; {foo bar} -> {\tt foo bar\/}
;;;; In regular text: Replace pairs of " with '' and ´´?
;;;; In regular text: Replace - with -- or --- based on context?
;;;; Requires: \bold[#1] \italic[#1] \code[#1] \url[#1]
;;;; \listing{#1} \item[#1]
;;;; \table[#1]{#2} \head{#1} \row{#1} \column[#1]
;;;; \graphicfigure[#1]{#2}
;;;; \textfigure[#1]{#2}
;;;; \section[#1] \subsection[#1] \subsubsection[#1] ...

;;;; Export document to TeX manuscript.

(defpackage document.export.tex
  (:documentation
   "Export document to TeX document.")
  (:use :cl
	:document
        :named-readtables
	:texp)
  (:export :export-document-tex
	   :export-document-index-tex))

(in-package :document.export.tex)

(in-readtable texp:syntax)

(defvar *section-level* 0
  "Section level.")

(defun print-text-markup (text-part)
  "Print TeX macro call for marked up TEXT-PART."
  (let ((text-part-string (escape (content-values text-part))))
    (case (content-type text-part)
      (#.+bold+   (tex (bold   {($ text-part-string)})))
      (#.+italic+ (tex (italic {($ text-part-string)})))
      (#.+code+   (tex (code   {($ text-part-string)})))
      (#.+url+    (tex (url    {($ text-part-string)})))
      (otherwise  (error "TEXT-PART has invalid content-type: ~S."
			 (content-type text-part))))))

(defun print-text (text)
  "Print TEXT in TeX representation."
  (dolist (text-part text)
    (if (stringp text-part)
	(write-string (escape text-part))
	(print-text-markup text-part)))
  (values))

(defun print-paragraph (paragraph)
  "Print PARAGRAPH in TeX representation."
  (print-text (content-values paragraph))
  (tex (br)))

(defun print-listing (listing)
  "Print LISTING in TeX representation."
  (tex (listing
	{($ (dolist (item (content-values listing))
	      (tex (item {($ (print-text item))}))))})
       (br)))

(defun print-table-row (row)
  "Print ROW in TeX representation."
  (dolist (column row)
    (tex (column [($ (print-text column))]))))

(defun print-table (table)
  "Print TABLE in TeX representation."
  (multiple-value-bind (description rows)
      (content-values table)
    (tex (table
	  [($ (print-text description))]
	  {(head {($ (print-table-row (first rows)))})
	  ($ (dolist (row (rest rows))
	       (tex (row {($ (print-table-row row))}))))}))))

(defun print-media (media-object)
  "Print MEDIA in TeX representation (can only be 2D graphics)."
  (multiple-value-bind (description url)
      (content-values media-object)
    (tex (graphicfigure [($ (print-text description))]
			{($ (escape url))}))))

(defun print-code (code-object)
  "Print MEDIA in TeX representation (can only be 2D graphics)."
  (multiple-value-bind (description text)
      (content-values code-object)
    (tex (textfigure [($ (print-text description))]
		     {($ (escape text))}))))

(defun print-header (header)
  "Print HEADER in TeX representation."
  (ecase *section-level*
    (0 (tex (section [($ (print-text header))])))
    (1 (tex (subsection [($ (print-text header))])))
    (2 (tex (subsubsection [($ (print-text header))])))
    (3 (tex (subsubsubsection [($ (print-text header))])))
    (4 (tex (subsubsubsubsection [($ (print-text header))])))
    (5 (tex (subsubsubsubsubsection [($ (print-text header))]))))
  (tex (br)))

(defun print-section (section)
  "Print SECTION in TeX representation."
  (multiple-value-bind (header contents)
      (content-values section)
    (print-header header)
    (let ((*section-level* (1+ *section-level*)))
      (print-contents contents))))

(defun print-content (content)
  "Print CONTENT in html representation."
  (case (content-type content)
    (#.+paragraph+ (print-paragraph content))
    (#.+listing+   (print-listing content))
    (#.+table+     (print-table content))
    (#.+media+     (print-media content))
    (#.+pre+       (print-code content))
    (#.+section+   (print-section content))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun print-contents (contents)
  "Print document or section CONTENTS in TeX representation."
  (dolist (content contents) (print-content content)))

(defun export-document-tex (document
			    &key (stream *standard-output*)
			         (section-level *section-level*))
  "Print DOCUMENT to STREAM as a TeX manuscript."
  (let ((*standard-output* stream)
	(*section-level* section-level))
    (print-contents document)))