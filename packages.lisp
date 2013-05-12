;;;; Package declarations for MK10.

(defpackage mk10
  (:documentation "MK10 core package. Includes support for
programatically creating and traversing as well as serializing MK10
documents.")
  (:use :cl)
  (:import-from :alexandria :flatten)
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
	   :walk-document
           :read-mk10
           :print-mk10))

(defpackage document.macros
  (:documentation "Macros to ease creating MK10 documents.")
  (:use :cl
        :mk10
	:named-readtables)
  (:export :paragraph
           :listing
           :table
           :media
           :pre
           :section
	   :document
	   :syntax))
