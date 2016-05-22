;;;; Read mk2 documents.

(in-package :geneva.mk2)

(defun read-mk2 (&optional (input *standard-input*))
  "*Arguments and Values:*

   _input_—a _string_ or _character stream_. The default is _standard
   input_.

   *Description:*

   {read-mk2} reads an _Mk2_ file from INPUT and returns a _document_.

   *Exceptional Situations:*

   If _input_ is not a valid _Mk2_ file an _error_ of _type_
   {syntax-error} is signaled.

   *See Also:*

   + syntax-error
    + [The Mk2 markup language](mk2.html)"
  (make-document (parse input (=document))))
