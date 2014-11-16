;;;; Open Geneva base packages.

(defpackage geneva
  (:documentation
   "Geneva core package. Exports functions to programatically create
    and inspect _Geneva documents_.

    In Open Geneva a _document_ is represented as a _list_ of _document
    elements_. A _document element_ can be obtained using the element
    constructors {make-paragraph}, {make-listing}, {make-table},
    {make-media} and {make-section}. In order to ensure integrity, it is
    recommended to use {make-document} to produce _documents_.

    _Rich text_ is represeted as a _list_ of _text tokens_. A _text
    token_ may be a _string_ or an object obtained using the text token
    constructors {make-bold}, {make-italic}, {make-fixed-width} and
    {make-url}.

    _Document elements_ and _text tokens_ can be inspected using the
    readers {content-type} and {content-values}.

    _Documents_ and _document elements_ are printable and readable using
    the Common Lisp printer and reader.

    *Exceptional Situations:*

    All functions external to this _package_ validate their parameters
    and will signal an _error_ of _type_ {type-error} on mismatch.

    *See Also:*

    + _Geneva Document Specification_ [geneva-document.html]
    + _Open Geneva User Manual_ [open-geneva.html]")
  (:use :cl
        :split-sequence)
  (:export :make-paragraph
	   :make-listing
	   :make-table
	   :make-media
	   :make-plaintext
	   :make-section
	   :make-bold
	   :make-italic
	   :make-fixed-width
	   :make-url
           :make-document
	   :content-type
	   :content-values))

(defpackage geneva.macros
  (:documentation
   "Macros and reader macros to help with procedural creation of Geneva
    documents.")
  (:use :cl
        :geneva
	:named-readtables)
  (:export :paragraph
           :listing
           :table
           :media
           :plaintext
           :section
	   :document
	   :syntax))
