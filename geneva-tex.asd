;;;; System definition for GENEVA-TEX.

(defsystem geneva-tex
  :description
  "Render Geneva documents as TeX manuscripts."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "tex"))
  :depends-on ("geneva" "texp" "file-types" "named-readtables"))
