;;;; System definition for document.export.html.

(defpackage document.export.html-asd
  (:use :cl :asdf))

(in-package :document.export.html-asd)

(defsystem document-export-html
  :components ((:file "export/html"))
  :depends-on ("document" "html" "file-types"))
