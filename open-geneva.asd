;;;; System definition for OPEN-GENEVA.

(defsystem open-geneva
  :description
  "Meta system for Open Geneva, an implementation of the Geneva document
   preparation system written in Common Lisp. This system pulls in all
   subsystems provided by Open Geneva."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :depends-on ("geneva"
               "geneva-mk2"
               "geneva-plain-text"
               "geneva-html"
               "geneva-tex"
               "geneva-latex"
               "geneva-cl"))
