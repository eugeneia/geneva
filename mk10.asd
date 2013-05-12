;;;; System definition for MK10.

(defpackage mk10-asd
  (:use :cl :asdf))

(in-package :mk10-asd)

(defsystem mk10
  :description "An easy to use text based document markup language with
  support for output formats suitable for web and print media."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "packages")
               (:file "mk10-struct"
                      :depends-on ("packages"))
               (:file "mk10-read"
                      :depends-on ("packages" "mk10-struct"))
               (:file "mk10-print"
                      :depends-on ("packages" "mk10-struct"))
	       (:file "mk10-macros"
                      :depends-on ("packages" "mk10-struct"))
	       (:file "mk10-syntax"
                      :depends-on ("packages"
                                   "mk10-struct"
                                   "mk10-macros")))
  :depends-on ("smug"
               "pretty-string"
               "split-sequence"
               "named-readtables"
               "alexandria"))
