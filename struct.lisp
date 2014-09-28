;;;; Internal data format for Geneva.

(in-package :geneva)

(defun make-markup (type string)
  "Make markup of TYPE for STRING. Signal a TYPE-ERROR if STRING is not
of type string."
  (check-type string string)
  (list type string))

(defun make-bold (string)
  "*Arguments and Values:*

   _string_—a _string_.

   *Description*:

   {make-bold} returns a _text token_ of type {:bold} for _string_."
  (make-markup :bold string))

(defun make-italic (string)
  "*Arguments and Values:*

   _string_—a _string_.

   *Description*:

   {make-italic} returns a _text token_ of type {:bold} for _string_."
  (make-markup :italic string))

(defun make-fixed-width (string)
  "*Arguments and Values:*

   _string_—a _string_.

   *Description*:

   {make-fixed-width} returns a _text token_ of type {:fixed-width} for
   _string_."
  (make-markup :fixed-width string))

(defun make-url (string)
  "*Arguments and Values:*

   _string_—a _string_.

   *Description*:

   {make-url} returns a _text token_ of type {:url} for _string_."
  (make-markup :url string))

(defun assert-text-token (thing)
  "Assert that THING is a valid text token. On failure signal a
TYPE-ERROR."
  (etypecase thing
    (list (check-type (first thing)
                      (member :bold :italic :fixed-width :url)))
    (string)))

(defun assert-rich-text (thing)
  "Assert that THING is a valid rich text sequence. On failure signal a
TYPE-ERROR."
  (check-type thing list)
  (loop for token in thing do (assert-text-token token)))

(defun make-paragraph (text)
  "*Arguments and Values:*

   _text_—a _rich text_ sequence.

   *Description*:

   {make-paragraph} returns _document element_ of type {:paragraph} with
   _text_."
  (assert-rich-text text)
  (list :paragraph (normalize-text text)))

(defun make-listing (items)
  "*Arguments and Values:*

   _items_—a _list_ of _rich text_ sequences.

   *Description*:

   {make-listing} returns a _document element_ of type {:listing} with
   _items_."
  (list :listing (loop for item in items
                    do (assert-rich-text item)
                    collect (normalize-text item))))

(defun make-object (type description &rest content)
  "Make an object of TYPE for DESCRIPTION text and CONTENT. Assert that
DESCRIPTION is a valid rich text sequence."
  (assert-rich-text description)
  `(,type ,(normalize-text description) ,@content))

(defun make-table (description rows)
  "*Arguments and Values:*

   _description_—a _rich text_ sequence.

   _rows_—a two dimensional list of _rich text_ sequences.

   *Description*:

   {make-table} returns a _document element_ of type {:table} with
   _description_ and _rows_."
  (make-object :table description
               (loop for row in rows
                  collect (loop for column in row
                             do (assert-rich-text column)
                             collect (normalize-text column)))))

(defun make-media (description url)
  "*Arguments and Values:*

   _description_—a _rich text_ sequence.

   _url_—a _string_.

   *Description*:

   {make-media} returns a _document element_ of type {:media} with
   _description_ and _url_."
  (check-type url string)
  (make-object :media description url))

(defun make-plaintext (description plaintext)
  "*Arguments and Values:*

   _description_—a _rich text_ sequence.

   _plaintext_—a _string_.

   *Description*:

   {make-plaintext} returns a _document element_ of type {:plaintext}
   with _description_ and _plaintext_."
  (check-type plaintext string)
  (make-object :plaintext description (normalize-plaintext plaintext)))

(defun assert-element (thing)
  "Assert that THING is a valid element. On failure signal a TYPE-ERROR."
  (check-type thing list)
  (check-type (first thing) (member :paragraph :listing :table
                                    :plaintext :media :section)))

(defun make-document (elements)
  "*Arguments and Values:*

   _elements_—a _list_ of _document elements_.

   *Description*:

   {make-document} returns a _document_ consisting of _elements_."
  (loop for thing in elements do (assert-element thing))
  (remove '(:paragraph nil) elements :test #'equal))

(defun make-section (header elements)
  "*Arguments and Values:*

   _header_—a _rich text_ sequence.

   _elements_—a _list_ of _document elements_.

   *Description*:

   {make-section} returns a _document element_ of type {section} with
   _header_ and _elements_."
  (make-object :section header (make-document elements)))

(defun assert-content (thing)
  "Assert that thing is a valid element or text token. On failure signal
a TYPE-ERROR."
  (etypecase thing
    (list (check-type (first thing)
                      (member :paragraph :listing :table
                              :plaintext :media :section
                              :bold :italic :fixed-width :url)))
    (string)))

(defun content-type (content)
  "*Arguments and Values:*

   _content_—an _element_ or a _text token_.

   *Description*:

   {content-type} returns a _keyword_ denoting the type of _content_
   which may be one of {:paragraph}, {:listing}, {:table}, {:plaintext},
   {:media}, {:section}, {:plain}, {:bold}, {:italic}, {:fixed-width} or
   {:url}."
  (assert-content content)
  (typecase content
    (list (first content))
    (string :plain)))

(defun content-values (content)
  "*Arguments and Values:*

   _content_—an _element_ or a _text token_.

   *Description*:

   {content-values} returns the components of _content_. The returned
   values are the _normalized_ forms of their respective content
   constructor's arguments and therefore depend on the type of
   _content_."
  (assert-content content)
  (typecase content
    (list (apply #'values (rest content)))
    (string content)))
