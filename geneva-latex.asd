;;;; System definition for GENEVA.LATEX.

(defsystem geneva-latex
  :components ((:file "latex"))
  :depends-on ("geneva" "geneva-tex" "texp" "named-readtables"))
