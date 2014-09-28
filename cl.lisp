;;;; Generate Geneva documents from Common Lisp inline documentation.

(defpackage geneva.common-lisp
  (:nicknames :geneva.cl)
  (:documentation
   "Render a document from an API extracted by PACKAGE-API.")
  (:use :cl
        :named-readtables
	:geneva
        :geneva.mk2
        :geneva.macros
        :trivial-documentation)
  (:export :api-document))

(in-package :geneva.common-lisp)

(in-readtable geneva.macros:syntax)

(defun name* (string-or-symbol)
  "Canonicalize STRING-OR-SYMBOL to _string_."
  (etypecase string-or-symbol
    (symbol (name* (symbol-name string-or-symbol)))
    (string (if (find-if #'lower-case-p string-or-symbol)
                string-or-symbol
                (string-downcase string-or-symbol)))))

(defun definition-template (kind-string name &optional text)
  "Template for common definition formatting."
  (document
   (make-paragraph
    `("— " ,kind-string ": " ,(make-bold name) " " ,@text))))

(defun docstring-document (docstring)
  "Compile document from DOCSTRING."
  (when docstring
    (read-mk2 docstring)))

(defun value-string (value)
  "Return pretty string for VALUE."
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-readably* t)
        (*print-escape* t)
        (*print-miser-width* *print-right-margin*))
    (handler-case (write-to-string value)
      (print-not-readable (c)
        (declare (ignore c))
        (let ((*print-readably* nil))
          (write-to-string value))))))

(defun render-variable (name variable-definition)
  "Renders VARIABLE-DEFINITION with NAME."
  (destructuring-bind (&key kind documentation value)
      variable-definition
    (append (definition-template
              (ecase kind
                (:variable "Variable")
                (:constant "Constant"))
                name)
            (document
             (paragraph #b"Initial Value:")
             (let ((value-string (value-string value)))
               (if (find #\Newline value-string)
                   (plaintext nil value-string)
                   (paragraph (make-fixed-width value-string)))))
            (docstring-document documentation))))

(defun render-lambda-list (lambda-list)
  "Render LAMBDA-LIST."
  (if (null lambda-list)
      (list #i"<no arguments>")
      (loop for head = lambda-list then (cdr head) while head
         for x = (car head)
         if (listp x)
         append `("(" ,@(render-lambda-list x) ")")
         else collect
           (case x
             ((&key &allow-other-keys &aux &body
               &environment &optional &rest &whole)
              (make-fixed-width (string-downcase (name* x))))
             (otherwise (make-italic (name* x))))
         when (cdr head) collect " ")))

(defun render-function (name function-definition)
  "Renders FUNCTION-DEFINITION with NAME."
  (destructuring-bind (&key kind documentation lambda-list)
      function-definition
    (append (definition-template
              (ecase kind
                (:function         "Function")
                (:generic-function "Generic function")
                (:macro            "Macro"))
              name
              (render-lambda-list lambda-list))
            (docstring-document documentation))))

(defun render-class-precedence-list (precedence-list)
  "Render class PRECEDENCE-LIST."
  (make-paragraph
   (loop for head = precedence-list then (cdr head) while head
      collect (make-fixed-width (name* (car head)))
      when (cdr head) collect ", ")))

(defun render-initargs (initargs)
  "Render class INITARGS."
  (when initargs
    (render-lambda-list (list* '&key initargs))))

(defun render-class (name class-definition)
  "Renders CLASS-DEFINITION with NAME."
  (destructuring-bind (&key kind documentation precedence-list initargs)
      class-definition
    (declare (ignore kind))
    (append
     (if (member 'condition precedence-list)
         (definition-template "Condition Type" name)
         (definition-template "Class" name (render-initargs initargs)))
     (document (paragraph #b"Class Precedence List:")
               (render-class-precedence-list precedence-list))
     (docstring-document documentation))))

(defun render-type (name type-definition)
  "Renders TYPE-DEFINITION with NAME."
  (destructuring-bind (&key kind documentation)
      type-definition
    (append (definition-template
                (ecase kind
                  (:structure "Structure")
                  (:type "Type"))
                name)
            (docstring-document documentation))))

(defun render-definition (name definition)
  "Renders DEFINITION for NAME."
  (case (getf definition :kind)
    ((:variable :constant)
     (render-variable name definition))
    ((:function :generic-function :macro)
     (render-function name definition))
    (:class
     (render-class name definition))
    ((:structure :type)
     (render-type name definition))))

(defun render-symbol-definitions (symbol definitions)
  "Renders DEFINITIONS for SYMBOL."
  (let ((name (name* symbol)))
    (make-section (list name)
                  (loop for definition in definitions append
                       (render-definition name definition)))))

(defun render-package (package)
  "Compile section for PACKAGE."
  (make-section
   (list (name* (package-name package)))
   (multiple-value-bind (docstring definitions)
       (package-api package)
     (append (read-mk2 docstring)
             (loop for head = definitions then (cddr head) while head
                collect (render-symbol-definitions (car head)
                                                   (cadr head)))))))

(defun api-document (&rest packages)
  "Render api document for PACKAGES."
  (make-document (mapcar #'render-package packages)))
