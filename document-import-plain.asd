;;;; System definition for document.import.plain.

(defpackage document.import.plain-asd
  (:use :cl :asdf))

(in-package :document.import.plain-asd)

(defsystem document-import-plain
  :components ((:file "import/plain"))
  :depends-on ("document" "smug"))
