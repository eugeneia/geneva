;;;; Defines named readtable for text markup syntax.

(in-package :geneva.macros)

(defun make-markup-reader (constructor)
  "Returns function that reads string literal and applies CONSTRUCTOR."
  (lambda (stream subchar arg)
    `(quote ,(funcall constructor (read stream t)))))

(defreadtable syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\b (make-markup-reader #'make-bold))
  (:dispatch-macro-char #\# #\i (make-markup-reader #'make-italic))
  (:dispatch-macro-char #\# #\f (make-markup-reader #'make-fixed-width))
  (:dispatch-macro-char #\# #\u (make-markup-reader #'make-url))
  (:case :invert))

(defparameter syntax (find-readtable 'syntax)
  "*Description:*

   Readtable containing reader macros for markup literals. Defines {#B},
   {#I}, {#F} and {#U} to be expanded to code generating Geneva markup at
   read-time using _make-bold_, _make-italic_, _make-fixed-width_ and
   _make-url_ respectively.

   *Notes:*

   This readtable is registered as _geneva.macros:syntax_. In
   order to use it invoke {named-readtable}'s {in-readtable} like so:

   #code#
   (in-readtable geneva.macros:syntax)
   #

   *Examples:*

   #code#
   #b\"bold string\" ≡ (geneva:make-bold \"bold string\")
   #i\"italic string\" ≡ (geneva:make-italic \"italic string\")
   #f\"fixed-width string\" ≡ (geneva:make-fixed-width \"fixed-width string\")
   #u\"url string\" ≡ (geneva:make-url \"url string\")
   #

   *See Also:*

   + Named-Readtables ({editor-hints.named-readtables})")
