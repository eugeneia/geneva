;;;; System definition for MK10.TEX.

(defpackage mk10.tex-asd
  (:use :cl :asdf))

(in-package :mk10.tex-asd)

(defsystem document-export-tex
  :components ((:file "mk10-tex"))
  :depends-on ("mk10" "texp" "named-readtables"))
