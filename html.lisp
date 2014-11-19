;;;; Render Geneva documents as HTML.

(defpackage geneva.html
  (:documentation "Render Geneva documents as HTML.")
  (:use :cl
	:geneva
        :geneva.utilities
	:macro-html
	:macro-html.widgets
        :named-readtables
	:file-types)
  (:shadow :map
           :time)
  (:export :render-html
           :render-html-file))

(in-package :geneva.html)

(in-readtable macro-html:syntax)

(defparameter *id-prefix* "section"
  "Prefix for generated id strings.")

(defparameter *header-level* 0
  "Header level.")

(defun render-text (text)
  "Render TEXT as HTML."
  (dolist (text-token text)
    (ecase (content-type text-token)
      (:plain (text #1=(content-values text-token)))
      (:bold (b #1#))
      (:italic (i #1#))
      (:fixed-width (code #1#))
      (:url (a [:href #1#] #1#)))))

(defun render-paragraph (paragraph)
  "Render PARAGRAPH as HTML."
  (p (render-text (content-values paragraph))))

(defun render-listing (listing)
  "Render LISTING as HTML."
  (ul (dolist (item (content-values listing))
	(li (render-text item)))))

(defun render-object-description (description)
  "Render DESCRIPTION for an object as HTML."
  (when description
    (figcaption (render-text description))))

(defun render-table (table)
  "Render TABLE as HTML."
  (figure
   (multiple-value-bind (description rows)
       (content-values table)
     (table
      (thead (tr (dolist (header (first rows))
                   (th (render-text header)))))
      (tbody (dolist (row (rest rows))
               (tr (dolist (column row)
                     (td (render-text column)))))))
     (render-object-description description))))
  
(defun render-media (media-object)
  "Render MEDIA-OBJECT as HTML."
  (figure
   (multiple-value-bind (description url)
       (content-values media-object)
     (let ((tags (file-tags url)))
       (cond ((member :image tags)
              (img :alt (text-string description) :src url)
              (render-object-description description))
             ((member :video tags)
              (video [:src url :controls nil])
              (render-object-description description))
             ((member :audio tags)
              (audio [:src url :controls nil])
              (render-object-description description))
             (t
              (a [:href url] (render-text description))))))))

(defun render-plaintext (plaintext-object)
  "Render PLAINTEXT-OBJECT as HTML."
  (figure
   (multiple-value-bind (description plaintext)
       (content-values plaintext-object)
     (pre plaintext)
     (render-object-description description))))

(defun make-section-id-string (level)
  "Returns id string for section at LEVEL with leading *id-prefix*."
  (format nil "~a-~{~a~^-~}" *id-prefix* level))

(defun render-headline (headline &optional level)
  "Render HEADLINE as HTML. When *INDEX-HEADERS-P* and LEVEL are not NIL,
prefix headline with LEVEL."
  (when (and *index-headers-p* level)
    (span [:class "geneva-index"] (level-string level))
    (format t " "))
  (render-text headline))

(defun render-header (headline level)
  "Render header with HEADLINE and LEVEL as HTML."
  (flet ((render-h ()
	   (case (+ (length level) *header-level*)
	     (1 (h1 (render-headline headline level)))
	     (2 (h2 (render-headline headline level)))
	     (3 (h3 (render-headline headline level)))
	     (4 (h4 (render-headline headline level)))
	     (5 (h5 (render-headline headline level)))
	     (t (h6 (render-headline headline level))))))
    (header (a [:name (make-section-id-string level)] (render-h)))))

(defun render-section (section level)
  "Render SECTION as HTML."
  (multiple-value-bind (description contents)
      (content-values section)
    (let ((level (or level (null-level))))
      (section
       (render-header description level)
       (render-contents contents (descend-level level)))
      (incf-level level))))

(defun render-content (content level)
  "Render CONTENT as HTML."
  (case (content-type content)
    (:paragraph (render-paragraph content))
    (:listing   (render-listing content))
    (:table     (render-table content))
    (:media     (render-media content))
    (:plaintext (render-plaintext content))
    (:section   (render-section content level))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun render-contents (contents &optional (level (null-level)))
  "Render document or section CONTENTS as HTML, starting at LEVEL for
headlines."
  (dolist (content contents)
    (render-content content level)))

(defun render-title (title)
  "Render TITLE with respect to *HEADER-LEVEL*."
  (case *header-level*
    (0 (h1 title))
    (1 (h2 title))
    (2 (h3 title))
    (3 (h4 title))
    (4 (h5 title))
    (t (h6 title))))

(defun make-id-href-string (id-string)
  "Returns id href string for ID-STRING."
  (concatenate 'string "#" id-string))

(defmacro render-index-list (&body body)
  "Convenience macro for RENDER-INDEX."
  `(if *index-headers-p*
       (ol ,@body)
       (ul ,@body)))

(defun render-index (index)
  "Render INDEX as HTML."
  (render-index-list
   (dolist (section index)
     (destructuring-bind (level title subsections)
         section
       (li (a [:href (make-id-href-string
                      (make-section-id-string level))]
              (render-headline title))
           (when subsections
             (render-index subsections)))))))

(defun render-html (document &key (stream *standard-output*)
                                  title
                                  author
                                  date
                                  (index-p *index-p*)
                                  (index-caption *default-index-caption*)
                                  (index-headers-p *index-headers-p*)
                                  (header-level *header-level*)
                                  (id-prefix *id-prefix*))
  "*Arguments and Values:*

   _header-level_—an _unsigned integer_. The default is {0}.

   _id-prefix_—a _string_. The default is {\"section\"}.

   *Description:*

   {render-html} renders _document_ as HTML. _header-level_ controls the
   initial headline level. For instance a _header-level_ of {1} will
   cause the top level headlines to be rendered as {H2} elements and so
   forth. _Id-prefix_ is used as a prefix to {NAME} attribute values of
   HTML anchor elements.

    *See Also:*

    + _Common Rendering Interface_ [open-geneva.html#section-3-1]"
  (let ((*standard-output* stream)
	(*index-headers-p* index-headers-p)
        (*header-level* header-level)
        (*id-prefix* id-prefix))
    (when (or title author date)
      (header (when title
                (render-title title)
                (incf *header-level*))
              (when author
                (p author))
              (when date
                (p date))))
    (when index-p
      (let ((index (document-index document)))
        (when index 
          (nav (header (p (b index-caption)))
               (render-index index)))))
    (render-contents document)))

(defun render-html-file (document
                         &key (stream *standard-output*)
                              title
                              author
                              date
                              (index-p *index-p*)
                              (index-caption *default-index-caption*)
                              (index-headers-p *index-headers-p*)
                              stylesheets
                              (encoding :utf-8))
  "*Arguments and Values:*

   _stylesheets_—a _list_ of stylesheets applicable to
   {macro-html.widgets:html-widget-document}.

   _encoding_—a _keyword_ designating a valid character encoding
   (defaults to {:utf-8}).

   *Description:*

   {render-html-file} renders _document_ as a standalone HTML file. The
   resulting HTML file will use _stylesheets_ and declare its content to
   be in _encoding_.

    *See Also:*

    + _Common Rendering Interface_ [open-geneva.html#section-3-1]"
  (let ((*standard-output* stream))
    (html-widget-document
     title
     (lambda ()
       (render-html document
                    :title title
                    :author author
                    :date date
                    :index-p index-p
                    :index-caption index-caption
                    :index-headers-p index-headers-p
                    :header-level 0))
     :encoding encoding
     :stylesheets stylesheets)))
