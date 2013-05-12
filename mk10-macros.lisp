;;;; Macros for document construction.

(defpackage document.macros
  (:use :cl
        :document
	:named-readtables)
  (:export :paragraph
           :listing
           :table
           :media
           :pre
           :section
	   :document
	   :syntax))

(in-package :document.macros)

(defmacro paragraph (&rest text)
  "Make a paragraph."
  `(make-paragraph (list ,@text)))

(defmacro listing (&rest items)
  "Make a listing."
  `(make-listing (list ,@(loop for item in items
                            collect `(list ,@item)))))
  
(defmacro table ((&rest description) &rest rows)
  "Make a table."
  `(make-table
    (list ,@description)
    (list ,@(loop for row in rows
               collect `(list ,@(loop for column in row
                                   collect `(list ,@column)))))))

(defmacro media ((&rest description) url)
  "Make a media object."
  `(make-media (list ,@description) ,url))

(defmacro pre ((&rest description) code)
  "Make code object."
  `(make-pre (list ,@description) ,code))

(defmacro section ((&rest header) &rest document)
  "Make section."
  `(make-section (list ,@header)
		 (list ,@document)))

(defmacro document (&rest content)
  `(list ,@content))
