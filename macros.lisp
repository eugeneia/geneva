;;;; (Read) macros for document construction.

(in-package :geneva.macros)

(defmacro paragraph (&rest text)
  "*Arguments and Values:*

   _text_—_forms_ which evaluate to Geneva _text tokens_.

   *Description:*

   {paragraph} returns a Geneva paragraph made up of _text_ as if by
   {geneva:make-paragraph}.

   *Notes:*

   #code#
   (paragraph {text}*) ≡ (make-paragraph (list {text}*))
   #"
  `(make-paragraph (list ,@text)))

(defmacro listing (&rest items)
  "*Arguments and Values:*

   _items_—_forms_ which evaluate to Geneva _rich text_.

   *Description:*

   {listing} returns a Geneva listing of _items_ as if by
   {geneva:make-listing}.

   *Notes:*

   #code#
   (listing {items}*) ≡ (make-listing (list {items}*))
   #"
  `(make-listing (list ,@(loop for item in items
                            collect `(list ,@item)))))

(defmacro table ((&rest description) &rest rows)
  "*Arguments and Values:*

   _description_—_forms_ which evaluate to Geneva _text tokens_.

   _rows_—a list of column lists containing _forms_ which evaluate to
   Geneva _text tokens_.

   *Description:*

   {table} returns a Geneva table with _rows_ and _description_ as if by
   {geneva:make-table}.

   *Examples:*

   #code#
   (table (\"10° Celsius in various units.\")
    ((\"Fahrenheit\") ((prin1-to-string (+ (* 1.8 10) 32))))
    ((\"Kelvin\") ((prin1-to-string (+ 10 273.15)))))
   ≡ (make-table (list \"10° Celsius in various units.\")
                 (list (list \"Fahrenheit\")
                       (list (prin1-to-string (+ (* 1.8 10) 32))))
                 (list (list \"Kelvin\")
                       (list (prin1-to-string (+ 10 273.15)))))
   #"
  `(make-table
    (list ,@description)
    (list ,@(loop for row in rows
               collect `(list ,@(loop for column in row
                                   collect `(list ,@column)))))))

(defmacro media ((&rest description) url)
  "*Arguments and Values:*

   _description_—_forms_ which evaluate to Geneva _text tokens_.

   _url_—a _form_ which evaluates to a _string_ designating an URL.

   *Description:*

   {media} returns a Geneva _media element_ for _url_ with _description_
   as if by {geneva:make-media}.

   *Notes:*

   #code#
   (media ({description}*) {url})
   ≡ (make-media (list {description}*) {url})
   #"
  `(make-media (list ,@description) ,url))

(defmacro plaintext ((&rest description) plaintext)
  "*Arguments and Values:*

   _description_—_forms_ which evaluate to Geneva _text tokens_.

   _plaintext_—a _form_ which evaluates to a _string_.

   *Description:*

   {plaintext} returns a Geneva _plaintext element_ for _plaintext_ with
   _description_ as if by {geneva:make-plaintext}.

   *Notes:*

   #code#
   (plaintext ({description}*) {plaintext})
   ≡ (make-plaintext (list {description}*) {plaintext})
   #"
  `(make-plaintext (list ,@description) ,plaintext))

(defmacro section ((&rest header) &rest content)
  "*Arguments and Values:*

   _header_—_forms_ which evaluate to Geneva _text tokens_.

   _content_—_forms_ which evaluate to Geneva _elements_.

   *Description:*

   {section} returns a Geneva _section element_ with _header_ and
   _content_ as if by {geneva:make-section}.

   *Notes:*

   #code#
   (section ({header}*) {body}*)
   ≡ (make-section (list {header}*) (list {body}*))
   #"
  `(make-section (list ,@header)
		 (list ,@content)))

(defmacro document (&rest content)
  "*Arguments and Values:*

   _document_—_forms_ which evaluate to Geneva _elements_.

   *Description:*

   {section} returns a Geneva _docuent_ with _content_ as if by
   {geneva:make-document}.

   *Notes:*

   #code#
   (document {content}*) ≡ (make-document (list {content}*))
   #"
  `(make-document (list ,@content)))
