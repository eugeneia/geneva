;;;; System definition for GENEVA-CL.

(defsystem geneva-cl
  :long-name "geneva-common-lisp"
  :description
  "Generate Geneva documents from Common Lisp inline documentation."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "cl"))
  :depends-on ("named-readtables"
               "geneva"
               "geneva-mk2"
               "trivial-documentation"
               "cl-ppcre"
	       "cl-ppcre-unicode"))
