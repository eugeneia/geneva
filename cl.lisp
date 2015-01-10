;;;; Compile Geneva documents from Common Lisp on-line documentation.

(defpackage geneva.common-lisp
  (:nicknames :geneva.cl)
  (:documentation
   "Compile a Geneva _document_ from Common Lisp on-line documentation.")
  (:use :cl
        :named-readtables
	:geneva
        :geneva.mk2
        :geneva.macros
        :split-sequence
        :trivial-documentation)
  (:export :api-document
           :symbol-document))

(in-package :geneva.common-lisp)

(in-readtable geneva.macros:syntax)

(defun name* (string-or-symbol)
  "Canonicalize STRING-OR-SYMBOL to string."
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


(defun docstring-paragraphs (docstring)
  "Split DOCSTRING into Geneva paragraphs."
  ;; This is a haskish approach. Deal with it.
  (mapcar (lambda (p) (paragraph (format nil "~{~a~%~}" p)))
          (split-sequence "" (split-sequence #\Newline docstring)
                          :test 'equal)))

(defun docstring-document (docstring)
  "Compile document from DOCSTRING."
  (when docstring
    ;; Try parsing DOCSTRING as Mk2.
    (handler-case (read-mk2 docstring)
      (error (e)
        (declare (ignore e))
        ;; If that fails, fall back to basic paragraph formatting.
        (make-document (docstring-paragraphs docstring))))))

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
  "Render VARIABLE-DEFINITION with NAME."
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
         append `(,#f"(" ,@(render-lambda-list x) ,#f")")
         else collect
           (case x
             ((&key &allow-other-keys &aux &body
               &environment &optional &rest &whole)
              (make-fixed-width (string-downcase (name* x))))
             (otherwise (make-italic (name* x))))
         when (cdr head) collect " ")))

(defun render-function (name function-definition)
  "Render FUNCTION-DEFINITION with NAME."
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
  "Render CLASS-DEFINITION with NAME."
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
  "Render TYPE-DEFINITION with NAME."
  (destructuring-bind (&key kind documentation)
      type-definition
    (append (definition-template
                (ecase kind
                  (:structure "Structure")
                  (:type "Type"))
                name)
            (docstring-document documentation))))

(defun render-definition (name definition)
  "Render DEFINITION for NAME."
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
  "Render DEFINITIONS for SYMBOL."
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
  "*Arguments and Values:*

   _packages_—_packages_ or _string designators_ naming _packages_.

   *Description:*

   {api-document} renders the on-line documentation for the _external
   symbols_ of _packages_ as a Geneva document."
  (make-document (mapcar #'render-package packages)))

(defun symbol-document (symbol)
  "*Arguments and Values:*

   _symbol_—a _symbol_.

   *Description:*

   {symbol-document} renders the on-line documentation for _symbol_ as a
   Geneva document."
  (multiple-value-bind (header definitions)
      (content-values (render-symbol-definitions
                       symbol (symbol-definitions symbol)))
    (declare (ignore header))
    (make-document definitions)))
