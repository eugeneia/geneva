;;;; (Read) macros for document construction.

(in-package :geneva.macros)

(defmacro paragraph (&rest text)
  "Make a paragraph for TEXT."
  `(make-paragraph (list ,@text)))

(defmacro listing (&rest items)
  "Make a listing for text ITEMS."
  `(make-listing (list ,@(loop for item in items
                            collect `(list ,@item)))))

(defmacro table ((&rest description) &rest rows)
  "Make a table for DESCRIPTION text and ROWS (a list) of columns (also
lists) of text."
  `(make-table
    (list ,@description)
    (list ,@(loop for row in rows
               collect `(list ,@(loop for column in row
                                   collect `(list ,@column)))))))

(defmacro media ((&rest description) url)
  "Make a media object for DESCRIPTION text and URL."
  `(make-media (list ,@description) ,url))

(defmacro plaintext ((&rest description) plaintext)
  "Make plaintext object for DESCRIPTION text and PLAINTEXT."
  `(make-plaintext (list ,@description) ,plaintext))

(defmacro section ((&rest header) &rest document)
  "Make section for HEADER and DOCUMENT body."
  `(make-section (list ,@header)
		 (list ,@document)))

(defmacro document (&rest content)
  "Make document with CONTENT."
  `(make-document (list ,@content)))
