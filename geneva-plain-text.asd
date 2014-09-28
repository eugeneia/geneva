;;;; System definition for GENEVA-PLAIN-TEXT.

(defsystem geneva-plain-text
  :description
  "Render Geneva documents as plain text."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "plain-text"))
  :depends-on ("geneva" "geneva-mk2" "pretty-string"))
