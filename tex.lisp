;;;; Render Geneva document as TeX manuscript.

;;; Expects macros as implicated below:
;;;
;;; Text:
;;; \genbold{#1} \genitalic{#1} \genfixedwidth{#1} \genurl{#1}
;;; E.g.: \genbold{...} ...
;;;
;;; Listing:
;;; \genlisting{#1} \genitem{#1}
;;; E.g.: \genlisting{\genitem{...} ...}
;;;
;;; Table:
;;; \gentable{#1}{#2} \genhead{#1} \genrow{#1} \gencolumn{#1}
;;; E.g.: \gentable{Description...}{
;;;         \genrow{\genhead{...} ...}
;;;         \genrow{\gencolumn{...} ...}
;;;         ...
;;;       }
;;;
;;; Figures:
;;; \gengraphic{#1}{#2}
;;; \genverbatimstart
;;; \genverbatimend
;;; \genverbatimdescription{#1}
;;; E.g.:
;;;  (Graphic figure)   \gengraphic{...}{<URL>}
;;;  (Plaintext figure) \genverbatimstart ... \genverbatimend
;;;                     \genverbatimdescription{...}
;;;
;;; Sections:
;;; \gensection{#1} \gensubsection{#1} \gensubsubsection{#1}
;;; E.g.: \gensection{...} ...

(defpackage geneva.tex
  (:documentation
   "Render Geneva document as TeX manuscript.")
  (:use :cl
	:geneva
	:texp
        :named-readtables)
  (:export :render-tex))

(in-package :geneva.tex)

(in-readtable texp:syntax)

(defvar *section-level* 0
  "Section level.")

(defun print-text-markup (text-part)
  "Print TeX macro call for marked up TEXT-PART."
  (let ((text-part-string (escape (content-values text-part))))
    (case (content-type text-part)
      (#.+bold+        (tex (genbold       {($ text-part-string)})))
      (#.+italic+      (tex (genitalic     {($ text-part-string)})))
      (#.+fixed-width+ (tex (genfixedwidth {($ text-part-string)})))
      (#.+url+         (tex (genurl        {($ text-part-string)})))
      (otherwise       (error "TEXT-PART has invalid content-type: ~S."
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
  (tex (genlisting
	{($ (dolist (item (content-values listing))
	      (tex (genitem {($ (print-text item))}))))})
       (br)))

(defun print-table-row (row)
  "Print ROW in TeX representation."
  (dolist (column row)
    (tex (gencolumn {($ (print-text column))}))))

(defun print-table-headrow (headrow)
  "Print HEADROW in TeX representation."
  (dolist (column headrow)
    (tex (genhead {($ (print-text column))}))))

(defun print-table (table)
  "Print TABLE in TeX representation."
  (multiple-value-bind (description rows)
      (content-values table)
    (tex (gentable
	  {($ (print-text description))}
	  {(genrow {($ (print-table-headrow (first rows)))})
	   ($ (dolist (row (rest rows))
		(tex (genrow {($ (print-table-row row))}))))})
	 (br))))

(defun print-media (media-object)
  "Print MEDIA in TeX representation (can only be 2D graphics)."
  (multiple-value-bind (description url)
      (content-values media-object)
    (tex (gengraphic {($ (print-text description))}
		   {($ (escape url))})
	 (br))))

(defun print-plaintext (plaintext-object)
  "Print PLAINTEXT-OBJECT in TeX representation."
  (multiple-value-bind (description text)
      (content-values plaintext-object)
    (tex (genverbatimstart)
	 ($ (fresh-line))
	 ($ (escape text))
         ($ (fresh-line))
	 (genverbatimend)
	 (genverbatimdescription {($ (print-text description))})
	 (br))))

(defun print-header (header)
  "Print HEADER in TeX representation."
  (case *section-level*
    (0 (tex (gensection {($ (print-text header))})))
    (1 (tex (gensubsection {($ (print-text header))})))
    (otherwise (tex (gensubsubsection {($ (print-text header))}))))
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
    (#.+plaintext+ (print-plaintext content))
    (#.+section+   (print-section content))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun print-contents (contents)
  "Print document or section CONTENTS in TeX representation."
  (dolist (content contents) (print-content content)))

(defun render-tex (document &optional (stream *standard-output*)
                            &key      (section-level *section-level*))
  "Render Geneva DOCUMENT as TeX manuscript to STREAM."
  (let ((*standard-output* stream)
	(*section-level* section-level))
    (print-contents document)))
