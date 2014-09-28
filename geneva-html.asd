;;;; System definition for GENEVA-HTML.

(defsystem geneva-html
  :description
  "Render Geneva documents as HTML."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "html"))
  :depends-on ("geneva" "macro-html" "file-types"))
