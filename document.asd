;;;; System definition for document.

(defpackage document-asd
  (:use :cl :asdf))

(in-package :document-asd)

(defsystem document
  :components ((:file "document")
	       (:file "document-macros" :depends-on ("document"))
	       (:file "document-macros-syntax"
		:depends-on ("document" "document-macros")))
  :depends-on ("named-readtables"))
