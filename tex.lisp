;;;; Render Geneva document as TeX manuscript.

;;; Expects macros as implicated below:
;;;
;;; Text:
;;; \genbold{#1} \genitalic{#1} \genfixedwidth{#1} \genurl{#1}
;;; E.g.: \genbold{...} ...
;;;
;;; \gentinyparagraph{#1} (For very short paragraphs)
;;;; E.g. \gentinyparagraph{...} 
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
;;; \genfallbackfigure{#1}{#2}
;;; \genverbatimstart
;;; \genverbatimend
;;; \genverbatimdescription{#1}
;;; E.g.:
;;;  (Graphic figure)   \gengraphic{...}{<URL>}
;;;  (Fallback figure)  \genfallbackfigure{...}{<URL>}
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
        :file-types
        :named-readtables)
  (:export :render-tex))

(in-package :geneva.tex)

(in-readtable texp:syntax)

(defvar *section-level* 0
  "Section level.")

(defun render-text-token (text-token)
  "Render TEXT-TOKEN as TeX using macro calls for markup tokens."
  (ecase (content-type text-token)
    (:plain       (write-string #1=(escape (content-values text-token))))
    (:bold        (tex (genbold       {($ #1#)})))
    (:italic      (tex (genitalic     {($ #1#)})))
    (:fixed-width (tex (genfixedwidth {($ #1#)})))
    (:url         (tex (genurl        {($ #1#)})))))

(defun render-text (text)
  "Render TEXT in TeX representation."
  (dolist (text-token text)
    (render-text-token text-token))
  (values))

(defun tiny-paragraph-p (paragraph)
  "If PARAGRAPH contains less than 60 characters its a _tiny paragraph_."
  (< (loop for token in (content-values paragraph)
        if (stringp token) sum (length token)
        else sum (length (content-values token)))
     60))

(defun render-paragraph (paragraph)
  "Render PARAGRAPH in TeX representation."
  (if (tiny-paragraph-p paragraph)
      (tex (gentinyparagraph
            {($ (render-text (content-values paragraph)))}))
      (tex ($ (render-text (content-values paragraph)))
           (br))))

(defun render-listing (listing)
  "Render LISTING in TeX representation."
  (tex (genlisting
	{($ (dolist (item (content-values listing))
	      (tex (genitem {($ (render-text item))}))))})
       (br)))

(defun render-table-row (row)
  "Render ROW in TeX representation."
  (dolist (column row)
    (tex (gencolumn {($ (render-text column))}))))

(defun render-table-headrow (headrow)
  "Render HEADROW in TeX representation."
  (dolist (column headrow)
    (tex (genhead {($ (render-text column))}))))

(defun render-table (table)
  "Render TABLE in TeX representation."
  (multiple-value-bind (description rows)
      (content-values table)
    (tex (gentable
	  {($ (render-text description))}
	  {(genrow {($ (render-table-headrow (first rows)))})
	   ($ (dolist (row (rest rows))
		(tex (genrow {($ (render-table-row row))}))))})
	 (br))))

(defun render-media (media-object)
  "Render MEDIA in TeX representation."
  (multiple-value-bind (description url)
      (content-values media-object)
    (if (file-tags url :image)
        (tex (gengraphic {($ (render-text description))}
                         {($ (escape url))})
             (br))
        (tex (genfallbackfigure {($ (render-text description))}
                                {($ (escape url))})
             (br)))))

(defun render-plaintext (plaintext-object)
  "Render PLAINTEXT-OBJECT in TeX representation."
  (multiple-value-bind (description text)
      (content-values plaintext-object)
    (tex (genverbatimstart)
	 ($ (fresh-line))
	 ($ (escape text))
         ($ (fresh-line))
	 (genverbatimend)
	 (genverbatimdescription {($ (render-text description))})
	 (br))))

(defun render-header (header)
  "Render HEADER in TeX representation."
  (case *section-level*
    (0 (tex (gensection {($ (render-text header))})))
    (1 (tex (gensubsection {($ (render-text header))})))
    (otherwise (tex (gensubsubsection {($ (render-text header))}))))
  (tex (br)))

(defun render-section (section)
  "Render SECTION in TeX representation."
  (multiple-value-bind (header contents)
      (content-values section)
    (render-header header)
    (let ((*section-level* (1+ *section-level*)))
      (render-contents contents))
    (tex (br))))

(defun render-content (content)
  "Render CONTENT in html representation."
  (case (content-type content)
    (:paragraph (render-paragraph content))
    (:listing   (render-listing content))
    (:table     (render-table content))
    (:media     (render-media content))
    (:plaintext (render-plaintext content))
    (:section   (render-section content))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun render-contents (contents)
  "Render document or section CONTENTS in TeX representation."
  (dolist (content contents) (render-content content)))

(defun render-tex (document &key (stream *standard-output*)
                                 (section-level *section-level*))
  "Render Geneva DOCUMENT as TeX manuscript to STREAM."
  (let ((*standard-output* stream)
	(*section-level* section-level))
    (render-contents document)))
