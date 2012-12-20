;;;; System definition for document.export.org-mode.

(defpackage document.export.org-mode-asd
  (:use :cl :asdf))

(in-package :document.export.org-mode-asd)

(defsystem document-export-org-mode
  :components ((:file "export/plain")
	       (:file "export/org-mode"
		      :depends-on ("export/plain")))
  :depends-on ("document"))
