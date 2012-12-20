;;;; Simple documents.

(defpackage document
  (:use :cl)
  (:export :+paragraph+
	   :+listing+
	   :+table+
	   :+media+
	   :+pre+
	   :+section+
	   :+bold+
	   :+italic+
	   :+code+
	   :+url+
	   :make-paragraph
	   :make-listing
	   :make-table
	   :make-media
	   :make-pre
	   :make-section
	   :make-bold
	   :make-italic
	   :make-code
	   :make-url
	   :content-type
	   :content-values
	   :walk-document))

(in-package :document)

(defconstant +paragraph+ :p
  "Paragraph symbol.")

(defconstant +listing+ :l
  "List symbol.")

(defconstant +table+ :t
  "Table symbol.")

(defconstant +media+ :m
  "Media symbol.")

(defconstant +pre+ :r
  "Pre symbol.")

(defconstant +section+ :s
  "Section symbol.")

(defconstant +bold+ :b
  "Bold symbol.")

(defconstant +italic+ :i
  "Italic symbol.")

(defconstant +code+ :c
  "Code symbol.")

(defconstant +url+ :u
  "URL symbol.")

(defun make-paragraph (text)
  "Make a paragraph."
  (list +paragraph+ text))

(defun make-listing (items)
  "Make a listing."
  (list +listing+ items))

(defun make-table (description rows)
  "Make a table."
  (list +table+ description rows))

(defun make-media (description url)
  "Make a media object."
  (list +media+ description url))

(defun make-pre (description code)
  "Make code object."
  (list +pre+ description code))

(defun make-section (header document)
  "Make section."
  (list +section+ header document))

(defun make-bold (string)
  "Make bold text."
  (list +bold+ string))

(defun make-italic (string)
  "Make italic text."
  (list +italic+ string))

(defun make-code (string)
  "Make fixed-width text."
  (list +code+ string))

(defun make-url (string)
  "Make URL link."
  (list +url+ string))

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