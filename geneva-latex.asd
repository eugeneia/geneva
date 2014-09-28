;;;; System definition for GENEVA.LATEX.

(defsystem geneva-latex
  :description
  "Render Geneva documents as LaTeX manuscripts."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "latex"))
  :depends-on ("geneva" "geneva-tex" "texp" "named-readtables"))
