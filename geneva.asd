;;;; System definition for GENEVA.

(defsystem geneva
  :description
  "Core of the Geneva document preparation system. Provides data
  structures and syntax sugar."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "geneva")
               (:file "normalize"
                      :depends-on ("geneva"))
               (:file "struct"
                      :depends-on ("geneva"))
	       (:file "macros"
                      :depends-on ("geneva" "struct"))
	       (:file "syntax"
                      :depends-on ("geneva"
                                   "struct"
                                   "macros"))
               (:file "utilities"
                      :depends-on ("geneva" "struct")))
  :depends-on ("split-sequence" "named-readtables"))
