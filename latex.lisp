;;;; Render Geneva document as LaTeX manuscript.

(defpackage geneva.latex
  (:documentation
   "Render Geneva document as LaTeX manuscript.")
  (:use :cl
	:named-readtables
	:texp
	:geneva.tex
        :geneva.utilities)
  (:export :render-latex))

(in-package :geneva.latex)

(in-readtable texp:syntax)

(defparameter *index-headers-p* t
  "Controls wether headers are numbered.")

(defun latex-text ()
  "Text formatting implementation for LaTeX."
  (deftex genbold       (text) (textbf {($ text)}))
  (deftex genitalic     (text) (textit {($ text)}))
  (deftex genfixedwidth (text) (texttt {($ text)}))
  (deftex genurl        (text) (underline  {($ text)}))

  (deftex gentinyparagraph (text)
    (medskip)
    (br)
    (noindent)
    ($ text)
    (br)
    (medskip)))

(defun latex-listing ()
  "Listing implementation for LaTeX."
  (deftex genitem (text) (item ($ text)))
  (deftex genlisting (items)
    (begin {itemize})
    ($ items)
    (end {itemize})))

(defun latex-figure ()
  "Figure implementation for LaTeX."
  (deftex genfigure (description content)
    (begin {figure} [h!])
    (centering)
    ($ content)
    (caption {($ description)})
    (end {figure})))

(defun latex-table ()
  "Table implementaton for LaTeX."
  (deftex gencolumn (text) ($ text) " & ")
  (deftex genhead (text) (genbold {($ text)}) " & ")
  (deftex genrow (columns) ($ columns) "\\\\[0.5em]")
  (deftex gentable (description rows)
    (genfigure {($ description)}
               {(begin {tabular}
                       {llllllllllll}) ;; hack
               ($ rows)
               (end {tabular})})))

(defun latex-graphic-figure ()
  "Graphic figure implementation for LaTeX."
  (deftex gengraphic (description url)
    (genfigure {($ description)}
               {(includegraphics [width=0.8 (textwidth)]
                                 {($ url)})})))

(defun latex-text-figure ()
  "Text figure implementation for LaTeX."
  (deftex genverbatimstart ()
    (begin {figure} [h!])
    (begin {quote})
    (begin {alltt}))
  (deftex genverbatimend ()
    "\\end{alltt}"
    (end {quote})
    (vspace {-1em}))
  (deftex genverbatimdescription (description)
    (caption {($ description)})
    (end {figure})))

(defun latex-sections ()
  "Sections implementation for LaTeX."
  (if *index-headers-p*
      (progn
        (deftex gensection (header)
          (section {($ header)}))
        (deftex gensubsection (header)
          (subsection {($ header)}))
        (deftex gensubsubsection (header)
          (subsubsection {($ header)})))
      (progn
        (deftex gensection (header)
          (section* {($ header)}))
        (deftex gensubsection (header)
          (subsection* {($ header)}))
        (deftex gensubsubsection (header)
          (subsubsection* {($ header)})))))

(defun document-implementation ()
  "Implementation of the document primitives for LaTeX."
  (latex-text)
  (latex-listing)
  (latex-figure)
  (latex-table)
  (latex-graphic-figure)
  (latex-text-figure)
  (latex-sections))

(defun default-preamble ()
  "Minimal default preamble."
  (tex (documentclass {article})
       (usepackage {graphicx})
       (usepackage {alltt})))

(defun render-latex (document
                     &key (stream *standard-output*)
                          title
                          author
                          (date :today)
                          (index-p t)
                          (index-caption *default-index-caption*)
                          (index-headers-p *index-headers-p*)
                          (preamble #'default-preamble)
                          appendix)
  "Render Geneva document as LaTeX manuscript."
  (let ((*standard-output* stream)
        (*index-headers-p* index-headers-p))
    (document-implementation)
    (when preamble (funcall preamble))
    (tex (br)
         (begin {document})
         (pagenumbering {roman}))
    (when title (tex (title {($ title)})))
    (when author (tex (author {($ author)})))
    (cond ((eq date :today) (tex (date {(today)})))
          ((stringp date) (tex (date {($ date)})))
          ((not date) (tex (date {}))))
    (when title (tex (maketitle)))
    (when index-p (tex (renewcommand {(contentsname)}
                                     {($ index-caption)})
                       (tableofcontents)
                       (newpage)))
    (tex (pagenumbering {arabic})
         (setcounter {page} {1})
         (br))
    (render-tex document)
    (when appendix (funcall appendix))
    (tex (end {document}))))
