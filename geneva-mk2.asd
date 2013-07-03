;;;; System definition for the mk2 markup language.

(defsystem geneva-mk2
  :description
  "Plain text markup language for the Geneva document preparation
  system."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "GNU AGPL"
  :components ((:file "mk2")
               (:file "tokens"
                      :depends-on ("mk2"))
               (:file "errors"
                      :depends-on ("mk2"))
               (:file "grammar"
                      :depends-on ("mk2"
                                   "errors"))
               (:file "read"
                      :depends-on ("mk2"
                                   "tokens"
                                   "errors"
                                   "grammar"))
               (:file "print"
                      :depends-on ("mk2"
                                   "tokens")))
  :depends-on ("smug" "pretty-string" "split-sequence"))
