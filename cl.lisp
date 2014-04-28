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
        :trivial-documentation
	:cl-ppcre)
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

(defparameter *non-symbol-characters*
  '(:whitespace-char-class
    #\( #\) #\' #\# #\` #\, #\; #\")
  "Non-symbol characters.")

(defparameter *manual-markup-characters*
  '(#\{ #\} #\[ #\] #\_)
  "Characters used for symbol indication by document markup.")

(defparameter *lower-case-characters*
  '((:property "LowercaseLetter"))
  "Lower case characters.")

(defparameter *other-grammar-tokens*
  '(#\, #\: #\; #\. #\! #\?)
  "Grammar tokens commonly used in sentences.")

(defparameter *most-symbols-regex*
  (create-scanner `(:alternation
                    ;; A string already wrapped in braces.
		    (:sequence ,(first *manual-markup-characters*)
			       (:greedy-repetition
                                0 nil
                                (:inverted-char-class
                                 ,(second *manual-markup-characters*)))
                               ,(second *manual-markup-characters*))
                    ;; A SYMBOL to be marked up (at least two uppercase
                    ;; symbol characters).
		    (:sequence
		     (:greedy-repetition
		      2 nil
		      (:inverted-char-class
		       ,@(append *non-symbol-characters*
				 *manual-markup-characters*
				 *lower-case-characters*)))
		     (:positive-lookahead
		      (:alternation ,@*other-grammar-tokens*
                                    ,@*non-symbol-characters*
                                    :end-anchor)))))
  "Regular expression to match most symbols or already marked up
strings.")

(defun most-symbols-replace (target-string start end
			     match-start match-end
			     reg-starts reg-ends)
  "Replace most symbols with markup but skip already marked up stings."
  (declare (ignore start end reg-starts reg-ends))
  (cond
    ;; TARGET-STRING is already marked up, return unchanged.
    ((char= (aref target-string match-start)
            (first *manual-markup-characters*))
     (subseq target-string match-start match-end))
    ;; TARGET-STRING ends in a character of *OTHER-GRAMMAR-TOKENS*, wrap
    ;; everything but the last character in braces.
    ((member (aref target-string (1- match-end)) *other-grammar-tokens*)
     (format nil "_~a_~c"
             (name* (subseq target-string match-start (1- match-end)))
             (aref target-string (1- match-end))))
    ;; Otherwise wrap TARGET-STRING in braces.
    (t (format nil "_~a_"
               (name* (subseq target-string match-start match-end))))))

;;; Markup here means mk2's italic font markup (_..._)
(defun markup-most-symbols (documentation-string)
  "Markup most symbols in DOCUMENTATION-STRING using
*MOST-SYMBOLS-REGEX*"
  (regex-replace-all *most-symbols-regex*
		     documentation-string
		     #'most-symbols-replace))

(defun definition-template (kind-string name &optional text)
  "Template for common definition formatting."
  (document
   (make-paragraph
    `("— " ,kind-string ": " ,(make-bold name) " " ,@text))))

(defun docstring-document (docstring)
  "Compile document from DOCSTRING."
  (when docstring
    (list* (paragraph "Description:")
           (read-mk2 (markup-most-symbols docstring)))))

(defun value-string (value)
  "Return pretty string for VALUE."
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-readably* t)
        (*print-escape* t)
        (*print-miser-width* *print-right-margin*))
    (write-to-string value)))

(defun render-variable (name variable-definition)
  "Renders VARIABLE-DEFINITION with NAME."
  (destructuring-bind (&key kind documentation value)
      variable-definition
    (append (definition-template
              (ecase kind
                (:variable "Variable")
                (:constant "Constant"))
                name)
            (document (paragraph "Initial Value:")
                      (make-plaintext nil (value-string value)))
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
    (append (definition-template "Class" name (render-initargs initargs))
            (document (paragraph "Class Precedence List:")
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
     (append (read-mk2 (markup-most-symbols docstring))
             (loop for head = definitions then (cddr head) while head
                collect (render-symbol-definitions (car head)
                                                   (cadr head)))))))

(defun api-document (&rest packages)
  "Render api document for PACKAGES."
  (make-document (mapcar #'render-package packages)))
