;;;; System definition for GENEVA.TEX.

(defsystem geneva-tex
  :components ((:file "tex"))
  :depends-on ("geneva" "texp" "named-readtables"))
