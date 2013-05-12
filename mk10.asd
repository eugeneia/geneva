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
               (:file "struct"
                      :depends-on ("packages"))
               (:file "read"
                      :depends-on ("packages"
                                   "struct"
                                   "tokens"))
               (:file "print"
                      :depends-on ("packages"
                                   "struct"
                                   "tokens"))
	       (:file "macros"
                      :depends-on ("packages" "struct"))
	       (:file "syntax"
                      :depends-on ("packages"
                                   "struct"
                                   "macros")))
  :depends-on ("smug"
               "pretty-string"
               "split-sequence"
               "named-readtables"
               "alexandria"))
