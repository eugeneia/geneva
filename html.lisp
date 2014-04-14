;;;; Render Geneva document as HTML.

(defpackage geneva.html
  (:documentation "Render Geneva document as HTML.")
  (:use :cl
        :named-readtables
	:geneva
	:macro-html
	:macro-html.widgets
	:file-types)
  (:shadow :map
           :time)
  (:export :render-html
           :render-index-html
           :render-document-html
           :render-document-html-file))

(in-package :geneva.html)

(in-readtable macro-html:syntax)

(defvar *header-level* 0
  "Header level.")

(defparameter *id-prefix* "section"
  "Prefix for generated id strings.")

(defvar *render-indexed-p* nil
  "Switch if headers are rendered indexed.")

(defun render-text (text)
  "Render TEXT as HTML."
  (dolist (text-part text)
    (if (stringp text-part)
	(write-string text-part)
	(let ((text-part-string (content-values text-part)))
	  (case (content-type text-part)
	    (#.+bold+ (b text-part-string))
	    (#.+italic+ (i text-part-string))
	    (#.+fixed-width+ (code text-part-string))
	    (#.+url+ (a [:href text-part-string] text-part-string))
	    (t (error "TEXT-PART has invalid content-type: ~S."
		      (content-type text-part))))))))

(defun render-text-lossy (text)
  "Render TEXT without associated style information."
  (dolist (text-part text)
    (if (stringp text-part)
	(write-string text-part)
	(write-string (content-values text-part)))))

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
              (img :alt (with-output-to-string (*standard-output*)
			  (render-text-lossy description))
		   :src url)
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

(defun print-level (level)
  "Print LEVEL."
  (format t "~{~a~^.~}" level))

(defun render-headline (headline &optional level)
  "Render HEADLINE as HTML. When *RENDER-INDEXED-P* and LEVEL are not
NIL, prefix headline with LEVEL."
  (when (and *render-indexed-p* level)
    (span [:class "geneva-index"] (print-level level))
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

(defun null-level ()
  "Returns the root level."
  (cons 1 nil))

(defun descend-level (level)
  "Returns the next deeper LEVEL."
  (append level (null-level)))

(defun incf-level (level)
  "Increment LEVEL by one."
  (incf (elt level (length (rest level)))))

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
    (#.+paragraph+ (render-paragraph content))
    (#.+listing+   (render-listing content))
    (#.+table+     (render-table content))
    (#.+media+     (render-media content))
    (#.+plaintext+ (render-plaintext content))
    (#.+section+   (render-section content level))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun render-contents (contents &optional (level (null-level)))
  "Render document or section CONTENTS as HTML, starting at LEVEL for
headlines."
  (dolist (content contents)
    (render-content content level)))

(defun render-html (document &key (stream *standard-output*)
                                  (render-indexed-p t)
                                  (id-prefix *id-prefix*)
                                  (header-level *header-level*))
  "Render DOCUMENT as HTML to STREAM. If RENDER-INDEXED-P is _true_
headlines are prefixed with a hierarchical index. ID-PREFIX is a string
prepended to HTML ids and defaults to {\"geneva\"}. HEADER-LEVEL controls
the initial headline level and defauls to 0."
  (let ((*standard-output* stream)
	(*render-indexed-p* render-indexed-p)
        (*id-prefix* id-prefix)
	(*header-level* header-level))
    (render-contents document)))

(defun make-id-href-string (id-string)
  "Returns id href string for ID-STRING."
  (concatenate 'string "#" id-string))

(defun document-sections (document)
  "Returns list of sections in DOCUMENT."
  (remove-if (lambda (content)
               (not (eq (content-type content) +section+)))
             document))

(defmacro render-index-list (&body body)
  "Convenience macro for RENDER-INDEX."
  `(if *render-indexed-p*
       (ol ,@body)
       (ul ,@body)))

(defun render-index (sections level)
  "Render index for SECTIONS at LEVEL as HTML."
  (render-index-list
   (dolist (section sections)
     (multiple-value-bind (description contents)
         (content-values section)
       (li (a [:href (make-id-href-string
                      (make-section-id-string level))]
	      (render-headline description))
           (let ((child-sections (document-sections contents)))
             (when child-sections
               (render-index child-sections (descend-level level))))))
     (incf-level level))))

(defun render-index-html (document &key (stream *standard-output*)
                                        (render-indexed-p t)
                                        (id-prefix *id-prefix*))
  "Render HTML index for DOCUMENT to STREAM. If RENDER-INDEXED-P is
_true_ headlines are prefixed with a hierarchical index. ID-PREFIX is a
string prepended to HTML ids and defaults to {\"geneva\"}."
    (let ((*standard-output* stream)
          (*id-prefix* id-prefix)
	  (*render-indexed-p* render-indexed-p)
          (level (null-level))
          (sections (document-sections document)))
      (when sections
        (nav
         (render-index sections level)))))

(defun render-document-html (document title
                             &key (stream *standard-output*)
                                  author
                                  index-caption
                                  index-p
                                  render-indexed-p)
  "Render DOCUMENT as HTML to STREAM."
  (let ((*standard-output* stream))
    (header (when author
              (p (em author)))
            (h1 title))
    (when index-p
      (aside
       (header (h2 index-caption))
       (render-index-html document
                          :render-indexed-p render-indexed-p)))
    (article
     (render-html document
                  :header-level 1
                  :render-indexed-p render-indexed-p))))

(defun render-document-html-file
    (document title
     &key (stream *standard-output*)
          stylesheets
          (encoding :utf-8)
          author
          (index-caption "Table of Contents")
          (index-p t)
          (render-indexed-p t))
  "Render DOCUMENT as HTML."
  (let ((*standard-output* stream))
    (html-widget-document
     title
     (lambda ()
       (render-document-html document title
                             :author author
                             :index-caption index-caption
                             :index-p index-p
                             :render-indexed-p render-indexed-p))
     :encoding encoding
     :stylesheets stylesheets)))
