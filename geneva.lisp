;;;; Packages for Geneva.

(defpackage geneva
  (:documentation
   "Geneva core package. Exports functions to programatically create and
traverse Geneva document.")
  (:use :cl
        :split-sequence)
  (:export :+paragraph+
	   :+listing+
	   :+table+
	   :+media+
	   :+plaintext+
	   :+section+
	   :+bold+
	   :+italic+
	   :+fixed-width+
	   :+url+
	   :make-paragraph
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
	   :content-values
	   :walk-document))

(defpackage geneva.macros
  (:documentation "Reader macros to ease creating Geneva documents.")
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
