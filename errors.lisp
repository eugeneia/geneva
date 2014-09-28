;;;; Error conditions and routines used by the grammar of GENEVA.MK2.

(in-package :geneva.mk2)


;;; Syntax error conditions.

(define-condition syntax-error (error)
  ((line-position
    :type (unsigned integer)
    :initform (error "Must supply LINE-POSITION.")
    :initarg :line
    :reader line-position
    :documentation "Line position of SYNTAX-ERROR.")
   (character-position
    :type (unsigned integer)
    :initform (error "Must supply CHARACTER-POSITION.")
    :initarg :character
    :reader character-position
    :documentation "Character position of SYNTAX-ERROR."))
  (:report print-syntax-error)
  (:documentation
   "*Description:*

    The _type_ {syntax-error} consists of error conditions that occur
    during {read-mk2}. It denotes a syntax error in the input to
    {read-mk2}. The functions {line-position} and {character-position}
    can be used to retrieve the position where the error occurred.

    *See Also:*

    + character-position
    + line-position"))

(setf (documentation 'line-position 'function)
      "*Arguments and Values:*

       _syntax-error_—an _error_ of type {syntax-error}.

       *Description:*

       {line-position} returns a _positive integer_ specifying the line
       of input on which _syntax-error_ occured.

       *See Also:*

       + syntax-error")

(setf (documentation 'character-position 'function)
      "*Arguments and Values:*

       _syntax-error_—an _error_ of type {syntax-error}.

       *Description:*

       {character-position} returns a _positive integer_ specifying the
       character position in the line on which _syntax-error_ occured.

       *See Also:*

       + syntax-error")

(defun print-syntax-error (syntax-error
                           &optional (stream *error-output*))
  "Print SYNTAX-ERROR to STREAM (which defaults to *ERROR-OUTPUT*)."
  (format stream "~a at position ~a:~a."
          (type-of syntax-error)
          (line-position syntax-error)
          (character-position syntax-error)))
#| TEST (let ((err (make-instance 'syntax-error :line 10 :character 17)))
          (print-syntax-error err)
          (format t "~&~a~%" err))
   Should print "SYNTAX-ERROR at position 10:17." twice. |#

(define-condition malformed-element (syntax-error) ()
  (:documentation
   "*Description:*

    The _type_ {malformed-element} is an error condition of type
    {syntax-error}. It occurs during parsing a _table_, _media_ or
    _plaintext_ element.

    *See Also:*

    + syntax-error"))

(define-condition section-not-closed (syntax-error) ()
  (:documentation
   "Internal syntax error describing an unclosed section."))

(define-condition open-section (syntax-error) ()
  (:documentation
   "*Description:*

    The _type_ {open-section} is an error condition of type
    {syntax-error}. It denotes an unclosed section.

    *See Also:*

    + syntax-error"))

(define-condition unrecognized-input (syntax-error) ()
  (:documentation
   "*Description:*

    The _type_ {unrecognized-input} is an error condition of type
    {syntax-error}. It denotes that a portion of the input could not be
    interpreted as _Mk2_.

    *See Also:*

    + syntax-error"))


;;; Error signaling routine.

(defun =syntax-error (error)
  "Signal ERROR at current position."
  (=fail (multiple-value-bind (position line character)
             (get-input-position)
           (declare (ignore position))
           (error error :line line :character character))))
