;;;; System definition for MK10.HTML.

(defpackage mk10.html-asd
  (:use :cl :asdf))

(in-package :mk10.html-asd)

(defsystem mk10-html
  :components ((:file "mk10-html"))
  :depends-on ("document" "html" "file-types"))
