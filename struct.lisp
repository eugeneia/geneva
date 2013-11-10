;;;; Internal data format for Geneva.

(in-package :geneva)

(defconstant +paragraph+ :p
  "Paragraph symbol.")

(defconstant +listing+ :l
  "List symbol.")

(defconstant +table+ :t
  "Table symbol.")

(defconstant +media+ :m
  "Media symbol.")

(defconstant +plaintext+ :r
  "Plaintext symbol.")

(defconstant +section+ :s
  "Section symbol.")

(defconstant +bold+ :b
  "Bold symbol.")

(defconstant +italic+ :i
  "Italic symbol.")

(defconstant +fixed-width+ :f
  "Fixed-Width symbol.")

(defconstant +url+ :u
  "URL symbol.")

(defun make-paragraph (text)
  "Make a paragraph for TEXT."
  (list +paragraph+ (normalize-text text)))

(defun make-listing (items)
  "Make a listing for text ITEMS (a list)."
  (list +listing+ (loop for item in items
                     collect (normalize-text item))))

(defun make-object (type description &rest content)
  "Make an object of TYPE for DESCRIPTION text and CONTENT."
  `(,type ,(normalize-text description) ,@content))

(defun make-table (description rows)
  "Make a table for DESCRIPTION text and ROWS (a list) of columns (also
lists) of text."
  (make-object +table+ description
               (loop for row in rows
                  collect (loop for column in row
                             collect (normalize-text column)))))

(defun make-media (description url)
  "Make a media object for DESCRIPTION text and URL."
  (make-object +media+ description url))

(defun make-plaintext (description plaintext)
  "Make plaintext object for DESCRIPTION text and PLAINTEXT."
  (make-object +plaintext+ description
               (normalize-plaintext plaintext)))

(defun make-section (header document)
  "Make section for HEADER and DOCUMENT body."
  (make-object +section+ header document))

(defun make-markup (type string)
  "Make markup of TYPE for STRING."
  (list type string))

(defun make-bold (string)
  "Make bold text token for STRING.."
  (make-markup +bold+ string))

(defun make-italic (string)
  "Make italic text token for STRING."
  (make-markup +italic+ string))

(defun make-fixed-width (string)
  "Make fixed-width text token for STRING."
  (make-markup +fixed-width+ string))

(defun make-url (string)
  "Make URL link text token for STRING."
  (make-markup +url+ string))

(defun content-type (content)
  "Returns CONTENT's type."
  (first content))

(defun content-values (content)
  "Returns CONTENT's values."
  (apply #'values (rest content)))

(defun section-content (section)
  "Returns SECTION's content."
  (third section))

(defun walk-document (document function)
  "Walk over DOCUMENT and call (FUNCTION content) on content."
  (dolist (content document)
    (funcall function content)
    (when (eq (content-type content) +section+)
      (walk-document (section-content content) function))))
