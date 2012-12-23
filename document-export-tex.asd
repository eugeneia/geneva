;;;; System definition for document.export.tex.

(defpackage document.export.tex-asd
  (:use :cl :asdf))

(in-package :document.export.tex-asd)

(defsystem document-export-tex
  :components ((:file "export/tex"))
  :depends-on ("document" "texp" "named-readtables"))
