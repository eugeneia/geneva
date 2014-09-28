;;;; Error conditions and routines for the grammar used by MK10.READER.

(in-package :geneva.mk2)


;;; Syntax error conditions

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
  (:documentation "Generic syntax error."))

(defun print-syntax-error (syntax-error
                           &optional (stream *error-output*))
  "Print SYNTAX-ERROR to STREAM."
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
   "Syntax error describing a malformed object."))

(define-condition section-not-closed (syntax-error) ()
  (:documentation
   "Internal syntax error describing an unclosed section."))

(define-condition open-section (syntax-error) ()
  (:documentation
   "Syntax error describing an open section."))

(define-condition unrecognized-input (syntax-error) ()
  (:documentation
   "We don't know whats wrong. Should really never happen."))


;;; Error signaling routine

(defun =syntax-error (error)
  "Signal ERROR at current position."
  (=fail (multiple-value-bind (position line character)
             (get-input-position)
           (declare (ignore position))
           (error error :line line :character character))))
