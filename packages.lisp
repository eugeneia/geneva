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
	   :walk-document))

(defpackage mk10.tokens
  (:documentation "Tokens used by MK10.")
  (:use :cl)
  (:export :*section-start*
           :*section-end*
           :*listing-item*
           :*table-item*    
           :*object-delimeter*
           :*bold-directive*    
           :*italic-directive*
           :*code-directive-start*
           :*code-directive-end* 
           :*url-directive-start*
           :*url-directive-end*
           :*escape-directive*
           :*special-tokens*
           :*markup-directives*
           :*table-keyword*
           :*media-keyword*
           :*pre-keyword*))

(defpackage mk10.serialize
  (:documentation "Read and print MK10 documents.")
  (:use :cl
        :mk10
        :mk10.tokens
        :smug
        :smug.characters
        :pretty-string
        :split-sequence)
  (:export :read-mk10
           :syntax-error
           :line-position
           :character-position
           :print-mk10))

(defpackage mk10.macros
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
