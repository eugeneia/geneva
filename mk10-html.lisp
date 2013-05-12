;;;; Convert MK10 document to HTML document.

(defpackage mk10.html
  (:use :cl
        :named-readtables
	:mk10
	:html
	:html.widgets
	:file-types)
  (:shadow :map
           :time)
  (:export :print-mk10-html
           :print-mk10-html-index))

(in-package :document.export.html)

(in-readtable html:syntax)

(defvar *header-level* 0
  "Header level.")

(defvar *id-prefix* "section"
  "Prefix for generated id strings.")

(defvar *print-indexed-p* nil
  "Switch if headers are printed indexed.")

(defun print-text (text)
  "Print TEXT in html representation."
  (dolist (text-part text)
    (if (stringp text-part)
	(write-string text-part)
	(let ((text-part-string (content-values text-part)))
	  (case (content-type text-part)
	    (#.+bold+ (b text-part-string))
	    (#.+italic+ (i text-part-string))
	    (#.+code+ (code text-part-string))
	    (#.+url+ (a [:href text-part-string] text-part-string))
	    (t (error "TEXT-PART has invalid content-type: ~S."
		      (content-type text-part))))))))

(defun print-text-lossy (text)
  "Print TEXT without associated style information."
  (dolist (text-part text)
    (if (stringp text-part)
	(write-string text-part)
	(write-string (content-values text-part)))))

(defun print-paragraph (paragraph)
  "Print PARAGRAPH in html representation."
  (p (print-text (content-values paragraph))))

(defun print-listing (listing)
  "Print LISTING in html representation."
  (ul (dolist (item (content-values listing))
	(li (print-text item)))))

(defun print-object-description (description)
  "Print DESCRIPTION for an object in html representation."
  (when description
    (figcaption (print-text description))))

(defun print-table (table)
  "Print TABLE in html representation."
  (figure
   (multiple-value-bind (description rows)
       (content-values table)
     (table
      (thead (tr (dolist (header (first rows))
                   (th (print-text header)))))
      (tbody (dolist (row (rest rows))
               (tr (dolist (column row)
                     (td (print-text column)))))))
     (print-object-description description))))
  
(defun print-media (media-object)
  "Print MEDIA-OBJECT in html representation."
  (figure
   (multiple-value-bind (description url)
       (content-values media-object)
     (let ((tags (file-tags url)))
       (cond ((member :image tags)
              (img :alt (with-output-to-string (*standard-output*)
			  (print-text-lossy description))
		   :src url)
              (print-object-description description))
             ((member :video tags)
              (video [:src url :controls nil])
              (print-object-description description))
             ((member :audio tags)
              (audio [:src url :controls nil])
              (print-object-description description))
             (t
              (a [:href url] (print-text description))))))))

(defun print-code (code-object)
  "Print CODE-OBJECT in html representation."
  (figure
   (multiple-value-bind (description code)
       (content-values code-object)
     (pre code)
     (print-object-description description))))

(defun make-section-id-string (level)
  "Returns id string for section at LEVEL with leading *id-prefix*."
  (format nil "~a-~{~a~^-~}" *id-prefix* level))

(defun print-level (level)
  "Print LEVEL."
  (format t "~{~a~^.~}" level))

(defun print-headline (headline &optional level)
  "Print HEADLINE in html representation possibly enumerated with LEVEL."
  (when (and *print-indexed-p* level)
    (span [:class "section-index"] (print-level level))
    (format t " "))
  (print-text headline))

(defun print-header (headline level)
  "Print header with HEADLINE in html representation."
  (flet ((print-h ()
	   (case (+ (length level) *header-level*)
	     (1 (h1 (print-headline headline level)))
	     (2 (h2 (print-headline headline level)))
	     (3 (h3 (print-headline headline level)))
	     (4 (h4 (print-headline headline level)))
	     (5 (h5 (print-headline headline level)))
	     (t (h6 (print-headline headline level))))))
    (header (a [:name (make-section-id-string level)] (print-h)))))

(defun null-level ()
  "Returns the root level."
  (cons 1 nil))

(defun descend-level (level)
  "Returns the next deeper level."
  (append level (null-level)))

(defun increase-level (level)
  "Increase LEVEL by one."
  (incf (elt level (length (rest level)))))

(defun print-section (section level)
  "Print SECTION in html representation."
  (multiple-value-bind (description contents)
      (content-values section)
    (let ((level (or level (null-level))))
      (section
       (print-header description level)
       (print-contents contents (descend-level level))
       (increase-level level)))))

(defun print-content (content level)
  "Print CONTENT in html representation."
  (case (content-type content)
    (#.+paragraph+ (print-paragraph content))
    (#.+listing+   (print-listing content))
    (#.+table+     (print-table content))
    (#.+media+     (print-media content))
    (#.+pre+       (print-code content))
    (#.+section+   (print-section content level))
    (t (error "Invalid content type in CONTENT: ~S."
	      (content-type content)))))

(defun print-contents (contents &optional (level (null-level)))
  "Print document or section CONTENTS in html representation."
  (dolist (content contents)
    (print-content content level)))

(defun print-mk10-html (document &key (stream *standard-output*)
                                      (print-indexed-p t)
                                      (id-prefix *id-prefix*)
                                      (header-level *header-level*))
  "Print DOCUMENT to STREAM in html representation."
  (let ((*standard-output* stream)
	(*print-indexed-p* print-indexed-p)
        (*id-prefix* id-prefix)
	(*header-level* header-level))
    (print-contents document)))

(defun make-id-href-string (id-string)
  "Returns id href string for ID-STRING."
  (concatenate 'string "#" id-string))

(defun document-sections (document)
  "Returns list of sections in DOCUMENT."
  (remove-if (lambda (content)
               (not (eq (content-type content) +section+)))
             document))

(defmacro print-index-list (&body body)
  "Convenience macro for PRINT-INDEX."
  `(if *print-indexed-p*
       (ol ,@body)
       (ul ,@body)))

(defun print-index (sections level)
  "Print index for SECTIONS at LEVEL in html representation."
  (print-index-list
   (dolist (section sections)
     (multiple-value-bind (description contents)
         (content-values section)
       (li (a [:href (make-id-href-string
                      (make-section-id-string level))]
	      (print-headline description))
           (let ((child-sections (document-sections contents)))
             (when child-sections
               (print-index child-sections (descend-level level))))))
     (increase-level level))))
         

(defun print-mk10-html-index (document &key (stream *standard-output*)
                                            (id-prefix *id-prefix*)
                                            (print-indexed-p t))
  "Print index for DOCUMENT to STREAM in html representation."
    (let ((*standard-output* stream)
          (*id-prefix* id-prefix)
	  (*print-indexed-p* print-indexed-p)
          (level (null-level))
          (sections (document-sections document)))
      (when sections
        (nav
         (print-index sections level)))))
