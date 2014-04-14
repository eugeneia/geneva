;;;; System definition for GENEVA.HTML.

(defsystem geneva-html
  :components ((:file "html"))
  :depends-on ("geneva" "macro-html" "file-types"))
