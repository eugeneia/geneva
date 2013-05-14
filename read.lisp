;;;; Read MK10 documents.

(defpackage mk10.reader
  (:documentation "Reader for MK10 documents.")
  (:use :cl
        :mk10
        :mk10.tokens
        :smug
        :smug.characters))

(in-package :mk10.reader)

;;; Syntax error conditions

(define-condition syntax-error ()
  ((line-position
    :type integer
    :initform (error "Must supply LINE-POSITION.")
    :initarg :line
    :accessor line-position
    :documentation "Line position of SYNTAX-ERROR.")
   (character-position
    :type integer
    :initform (error "Must supply CHARACTER-POSITION.")
    :initarg :character
    :accessor character-position
    :documentation "Character position of SYNTAX-ERROR."))
  (:documentation "Generic syntax error."))

(define-condition unexpected-token (syntax-error)
  ((expected
    :type (or character string symbol)
    :initform nil
    :initarg :expected
    :reader expected-token
    :documentation "Expected token."))
  (:documentation "Unexpected token error."))

(define-condition unmatched-markup (unexpected-token) ()
  (:documentation "Unmatched markup error."))

(define-condition malformed-object (unexpected-token) ()
  (:documentation "Malformed object error."))

(define-condition malformed-media-object (unexpected-token) ()
  (:documentation "Malformed media object error."))

(define-condition malformed-section (unexpected-token) ()
  (:documentation "Malformed section error."))

(defmacro syntax-error (type &rest initargs)
  (let ((g!position (gensym "position"))
	(g!line (gensym "line"))
	(g!character (gensym "character")))
    `(=fail
      (multiple-value-bind (,g!position ,g!line ,g!character)
	(get-input-position)
      (declare (ignore ,g!position))
      (error ,type
	     :line ,g!line
	     :character ,g!character
	     ,@initargs)))))

(defmacro pass-syntax-error (syntax-error)
  (let ((g!syntax-error (gensym "sytax-error")))
    `(let ((,g!syntax-error ,syntax-error))
       (=fail
	(multiple-value-bind (position line character)
	  (get-input-position)
	  (declare (ignore position))
	  (let ((child-line (line-position ,g!syntax-error))
		(child-character (character-position ,g!syntax-error)))
	    (setf (line-position ,g!syntax-error)
		  (+ (1- line) child-line))
	    (setf (character-position ,g!syntax-error)
		  (if (= line child-line)
		      (+ (1- character) child-character)
		      child-character)))
	  (error ,g!syntax-error))))))


;;; Generic parsers

(defun =end-of-document ()
  (=skip-whitespace (=end-of-input)))

(defun =double-newline ()
  (let ((newline (=newline))
	(other-whitespace
	 (=zero-or-more (=unless (=newline) (=whitespace)))))
    (=and other-whitespace
	  newline
	  other-whitespace
	  newline)))

(defun =element-terminator ()
  (=or (=double-newline) (=end-of-document)))


;;; Text parsers

(defun =escape ()
  (=satisfies (lambda (character)
		(char= *escape-directive* character))))

(defun =escaped-character ()
  (=and (=escape) (=item)))

(defun =plain-character (not-one-of)
  (=or (=escaped-character)
       (=not (=or (=whitespace)
		  (=one-of not-one-of)
		  (=end-of-document)))))

(defun =token (character)
  (=unless (=escape)
	   (=character character)))

(defun =plain-word (not-one-of)
  (=let* ((leading-whitespace (=maybe (=one-or-more (=whitespace))))
	  (plain-characters
	   (=maybe (=string-of (=plain-character not-one-of)))))
    (if (or leading-whitespace plain-characters)
	(=result (concatenate 'string
			      (if leading-whitespace " " "")
			      (or plain-characters "")))
	(=fail))))

(defun =plain-text (not-one-of)
  (=let* ((text (=one-or-more (=plain-word not-one-of)))
	  (whitespace (=if (=end-of-document)
			   (=result nil)
			   (=zero-or-more (=whitespace)))))
    (=result (apply #'concatenate 'string
		   (append text (list (if whitespace " " "")))))))

(defun =markup-text (constructor start-directive
		     &optional (end-directive start-directive))
  (=funcall
   (=prog2 (=token start-directive)
	   (=plain-text (list end-directive))
	   (=or (=token end-directive)
		(=end-of-document)
		(syntax-error 'unmatched-markup
			      :expected end-directive)))
   constructor))

(defun =bold-text ()
  (=markup-text #'make-bold *bold-directive*))

(defun =italic-text ()
  (=markup-text #'make-italic *italic-directive*))

(defun =code-text ()
  (=markup-text #'make-code *code-directive-start* *code-directive-end*))

(defun =url-text ()
  (=markup-text #'make-url *url-directive-start* *url-directive-end*))

(defun =text-token ()
  (=or (=plain-text *markup-directives*)
       (=bold-text)
       (=italic-text)
       (=code-text)
       (=url-text)))

(defun =text ()
  (=let* ((text (=skip-whitespace (=zero-or-more (=text-token)))))
    (=result (if (equal (first (last text)) " ")
		 (butlast text)
		 text))))

(defun =string-until (until-parser)
  (=let* ((string-tree (=zero-or-more
			(=unless until-parser
				 (=or (=list (=escape)
					     (=item))
				      (=item))))))
    (=result (coerce (flatten string-tree) 'string))))
		   
(defun =text-until (until-parser)
  (=handler-case
   (=funcall (=string-until until-parser)
	     (lambda (string)
	       (run (=text) string)))
   (unexpected-token (syntax-error) (pass-syntax-error syntax-error))))

(defun =paragraph ()
  (=unless (=skip-whitespace (=one-of *special-tokens*))
	   (=funcall (=text-until (=element-terminator))
		     #'make-paragraph)))


;;;; Listing parsers

(defun =listing-item ()
  (=and (=token *listing-item*)
	(=text-until (=or (=token *listing-item*)
			 (=element-terminator)))))

(defun =listing ()
  (=funcall (=one-or-more
	     (=unless (=element-terminator)
		      (=skip-whitespace (=listing-item))))
	    #'make-listing))

(defun =object (keyword body-parser constructor)
  (=and (=token *object-delimeter*)
	(=string keyword nil)
	(=or (=whitespace)
	     (syntax-error 'malformed-object :expected :keyword))
	(=skip-whitespace
	 (=let* ((description (=text-until (=token *object-delimeter*)))
		 (_ (=or (=token *object-delimeter*)
			 (syntax-error 'malformed-object
				       :expected *object-delimeter*))))
	   (=funcall body-parser
		     (lambda (body)
		       (funcall constructor description body)))))))


;;;; Table parsers

(defun =table-column ()
  (=and (=token *table-item*)
	(=text-until (=or (=token *table-item*)
			  (=token #\Newline)
			  (=end-of-document)))))

(defun =table-row ()
  (=one-or-more
   (=unless (=or (=token #\Newline)
		 (=element-terminator))
	    (=skip-whitespace (=table-column)))))

(defun =table ()
  (=object *table-keyword*
	   (=zero-or-more (=skip-whitespace (=table-row)))
	   #'make-table))

(defun =media ()
  (=object *media-keyword*
	   (=or (=skip-whitespace (=string-until (=element-terminator)))
		(syntax-error 'malformed-media-object))
	   #'make-media))

(defun =code ()
  (let ((terminator (=and (=skip-whitespace (=token *object-delimeter*))
			  (=newline))))
    (flet ((=count-whitespace ()
	     (=let* ((whitespace (=zero-or-more (=whitespace))))
	       (let ((position
		      (position #\Newline (reverse whitespace))))
		 (=result (or position (length whitespace))))))
	   (=trim-space (parser n)
	     (=and (=zero-to n (=character #\Space))
		   parser))
	   (=trim-escape (parser)
	     (=funcall parser
		       (lambda (string)
			 (if (char= *escape-directive* (aref string 0))
			     (subseq string 1)
			     string)))))
      (=object *pre-keyword*
	       (=let* ((_ (=zero-or-more (=newline)))
		       (indent (=count-whitespace))
		       (body
			(=zero-or-more
			 (=unless terminator
				  (=trim-escape
				   (=trim-space (=line t) indent)))))
		       (_ (=maybe terminator)))
		 (=result (string-trim
			   '(#\Newline)
			   (apply #'concatenate 'string body))))
	       #'make-pre))))

(defun =section-body ()
  (=zero-or-more (=skip-whitespace (=element))))

(defun =section ()
  (=let* ((_ (=token *section-start*))
	  (header (=skip-whitespace (=text-until (=double-newline))))
	  (_ (=or (=double-newline)
		  (syntax-error 'malformed-section
				:expected :double-newline)))
	  (body (=section-body))
	  (_ (=or (=skip-whitespace (=token *section-end*))
		  (syntax-error 'malformed-section
				:expected *section-end*))))
    (=result (make-section header body))))


;;;; Element parsers

(defun =element ()
  (=skip-whitespace (=or (=paragraph)
			 (=listing)
			 (=table)
			 (=media)
			 (=code)
			 (=section))))

(defun =document ()
  (=prog1 (=section-body)
	  (=or (=end-of-document)
	       (syntax-error 'malformed-object))))


;;; Interface to the reader

(in-package :mk10.serialize)

(defun read-mk10 (&optional (input *standard-input*))
  "Parse MK10 document from INPUT."
  (run (mk10.reader::=document) input))
