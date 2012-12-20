;;;; System definition for document.export.plain.

(defpackage document.export.plain-asd
  (:use :cl :asdf))

(in-package :document.export.plain-asd)

(defsystem document-export-plain
  :components ((:file "export/plain"))
  :depends-on ("document"))
