;;;; Export document to TeX manuscript.

;;;; Requires these macros:
;;;; \dbold{#1} \ditalic{#1} \dcode{#1} \durl{#1}
;;;; \dlisting{#1} \ditem{#1}
;;;; \dtable{#1}{#2} \dhead{#1} \drow{#1} \dcolumn{#1}
;;;; \dgraphic{#1}{#2}
;;;; \dverbatimstart
;;;; \dverbatimend
;;;; \dverbatimdescription{#1}
;;;; \dsection{#1} \dsubsection{#1} \dsubsubsection{#1}

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
      (#.+bold+   (tex (dbold   {($ text-part-string)})))
      (#.+italic+ (tex (ditalic {($ text-part-string)})))
      (#.+code+   (tex (dcode   {($ text-part-string)})))
      (#.+url+    (tex (durl    {($ text-part-string)})))
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
  (tex (dlisting
	{($ (dolist (item (content-values listing))
	      (tex (ditem {($ (print-text item))}))))})
       (br)))

(defun print-table-row (row)
  "Print ROW in TeX representation."
  (dolist (column row)
    (tex (dcolumn {($ (print-text column))}))))

(defun print-table-headrow (headrow)
  "Print HEADROW in TeX representation."
  (dolist (column headrow)
    (tex (dhead {($ (print-text column))}))))

(defun print-table (table)
  "Print TABLE in TeX representation."
  (multiple-value-bind (description rows)
      (content-values table)
    (tex (dtable
	  {($ (print-text description))}
	  {(drow {($ (print-table-headrow (first rows)))})
	   ($ (dolist (row (rest rows))
		(tex (drow {($ (print-table-row row))}))))})
	 (br))))

(defun print-media (media-object)
  "Print MEDIA in TeX representation (can only be 2D graphics)."
  (multiple-value-bind (description url)
      (content-values media-object)
    (tex (dgraphic {($ (print-text description))}
		   {($ (escape url))})
	 (br))))

(defun print-code (code-object)
  "Print CODE-OBJECT in TeX representation."
  (multiple-value-bind (description text)
      (content-values code-object)
    (tex (dverbatimstart)
	 ($ (fresh-line))
	 ($ text)
	 ($ (fresh-line))
	 (dverbatimend)
	 (dverbatimdescription {($ (print-text description))})
	 (br))))

(defun print-header (header)
  "Print HEADER in TeX representation."
  (case *section-level*
    (0 (tex (dsection {($ (print-text header))})))
    (1 (tex (dsubsection {($ (print-text header))})))
    (otherwise (tex (dsubsubsection {($ (print-text header))}))))
  (tex (br)))

(defun print-section (section)
  "Print SECTION in TeX representation."
  (multiple-value-bind (header contents)
      (content-values section)
    (print-header header)
    (let ((*section-level* (1+ *section-level*)))
      (print-contents contents))
    (tex (br))))

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
