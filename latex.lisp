;;;; Render Geneva documents as LaTeX manuscripts.

(defpackage geneva.latex
  (:documentation
   "Render Geneva documents as LaTeX manuscripts.")
  (:use :cl
	:named-readtables
	:texp
	:geneva.tex
        :geneva.utilities)
  (:export :render-latex))

(in-package :geneva.latex)

(in-readtable texp:syntax)

(defun latex-text ()
  "Text formatting implementation for LaTeX."
  (deftex genbold       (text) (textbf {($ text)}))
  (deftex genitalic     (text) (textit {($ text)}))
  (deftex genfixedwidth (text) (texttt {($ text)}))
  (deftex genurl        (text) (url    {($ text)}))

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
    (raggedright)
    ($ items)
    (end {itemize})))

(defun latex-figure ()
  "Figure implementation for LaTeX."
  (deftex genfigure (description content)
    (begin {figure} ["H"])
    (centering)
    ($ content)
    (caption {($ description)})
    (end {figure})))

(defun latex-table ()
  "Table implementaton for LaTeX."
  (deftex gencolsep () " & ")
  (deftex gencolumn (text) ($ text))
  (deftex genhead (text) (genbold {($ text)}))
  (deftex genrow (columns) ($ columns) "\\\\[0.5em]")
  (deftex gentable (description format rows)
    (genfigure {($ description)}
               {(begin {tabularx} {(columnwidth)}
                       {($ format)})
               ($ rows)
               (end {tabularx})})))

(defun latex-graphic-figure ()
  "Graphic figure implementation for LaTeX."
  (deftex gengraphic (description url)
    (genfigure {($ description)}
               {(includegraphics [width=0.8 (columnwidth)]
                                 {($ url)})})))

(defun latex-fallback-figure ()
  "Fallback figure implementation for LaTeX."
  (deftex genfallbackfigure (description url)
    (genfigure {($ description)}
               {(texttt {($ url)})})))

(defun latex-text-figure ()
  "Text figure implementation for LaTeX."
  (deftex genverbatimstart ()
    (begin {figure} ["H"])
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
  (latex-fallback-figure)
  (latex-text-figure)
  (latex-sections))

(defun default-preamble ()
  "Minimal default preamble."
  (tex (documentclass {article})
       (usepackage {graphicx})
       (usepackage {tabularx})
       (usepackage {alltt})
       (usepackage {float})
       (usepackage [hyphens] {url})))

(defun render-latex (document
                     &key (stream *standard-output*)
                          title
                          author
                          (date :today)
                          (index-p *index-p*)
                          (index-caption *default-index-caption*)
                          (index-headers-p *index-headers-p*)
                          (preamble #'default-preamble)
                          appendix)
  "*Arguments and Values:*

   _preamble_—a _function_ without arguments that prints LaTeX
   expressions to {*standard-output*}. The produced LaTeX expressions
   will be inserted at the beginning of the LaTeX manuscript.

   _appendix_—a _function_ without arguments that prints LaTeX
   expressions to {*standard-output*}. The produced LaTeX expressions
   will be appended to the LaTeX manuscript.

   *Description:*

   {render-latex} renders _document_ as a LaTeX manuscript.  _Preamble_
   and _appendix_ may be supplied to customize the LaTeX layout and
   functionality. Their output will be inserted at the beginning or
   appended to the end of the LaTeX manuscript respectively.

   *See Also:*

    + _Common Rendering Interface_ [open-geneva.html#section-3-1]"
  (let ((*standard-output* stream)
        (*index-headers-p* index-headers-p))
    (document-implementation)
    (when preamble (funcall preamble))
    (tex (br)
         (begin {document})
         (pagenumbering {roman}))
    (tex (title {($ (or title ""))}))
    (when author (tex (author {($ author)})))
    (cond ((eq date :today) (tex (date {(today)})))
          ((stringp date) (tex (date {($ date)})))
          ((not date) (tex (date {}))))
    (when (or title author date)
      (tex (maketitle)))
    (when (and index-p (document-index document))
      (tex (renewcommand {(contentsname)}
                         {($ index-caption)})
           (tableofcontents)
           (bigskip)))
    (tex (pagenumbering {arabic})
         (setcounter {page} {1})
         (br))
    (render-tex document)
    (when appendix (funcall appendix))
    (tex (end {document}))))
