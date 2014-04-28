;;;; System definition for GENEVA.PLAIN-TEXT.

(defsystem geneva-plain-text
  :components ((:file "plain-text"))
  :depends-on ("geneva" "geneva-mk2" "pretty-string"))
