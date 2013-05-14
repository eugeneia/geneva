;;; LPC grammar used by MK10.READER.

(in-package :mk10.reader)

(defun =escape ()
  (=character *escape-directive*))

(defun =escaped-char ()
  (=and (=escape) (=item)))

(defun =plain-char ()
  (=or (=escaped-char)
       (=item)))

(defun =plain-text (until)
  (=string-of (=unless until (=plain-char))))

(defun =token (char)
  (=unless (=escaped-char)
           (=character char)))

(defun =markup% (constructor start &optional (end start))
  (=let* ((_ (=token start))
          (text (=plain-text (=token end)))
          (_ (=token end)))
    (=result (funcall constructor text))))

(defun =markup ()
  (=or
   (=markup% #'make-bold   *bold-directive*)
   (=markup% #'make-italic *italic-directive*)
   (=markup% #'make-code   *code-directive-start* *code-directive-end*)
   (=markup% #'make-url    *url-directive-start*  *url-directive-end*)))

(defun =markup-directive ()
  (=one-of *markup-directives*))

(defun =text (until)
  (=one-or-more
   (=or (=markup)
        (=text (=or (=markup-directive)
                    until)))))

(defun =double-newline ()
  (let ((newline (=newline))
	(other-whitespace
         (=zero-or-more (=unless (=newline) (=whitespace)))))
    (=and other-whitespace newline
	  other-whitespace newline)))

(defun =end-of-document ()
  (=skip-whitespace (=end-of-input)))

(defun =content-delimiter ()
  (=or (=double-newline)
       (=end-of-document)))

(defun =paragraph ()
  (=prog1 (=text (=content-delimiter))
          (=content-delimiter)))

(defun =list-item ()
  (=prog2 (=token *listing-item*)
          (=text (=or (=token *listing-item*)
                      (=content-delimiter)))))

(defun =listing ()
  (=prog1 (=one-or-more (=list-item))
          (=content-delimiter)))

;;; continue...

(defun =object ()
  (=or (=object% :media #'make-media (=url))
       (=object% :table #'make-table (=table-body))
       (=object% :code  #'make-code  (=code-body))))

(defun =section ()
  (=handler-case
   (=let* ((_ (=token *section-start*))
           (header (=paragraph))
           (contents (=contents))
           (_ (=or (=skip-whitespace (=token *section-end*))
                   ;; Sections must be closed aye.
                   (=syntax-error 'section-not-closed))))
     (=result (make-section header contents)))
   ;; We have an unclosed section so we signal where it opened.
   (section-not-closed () (=syntax-error 'open-section))))

(defun =contents ()
  ;; No content is valid content.
  (=zero-or-more
   ;; Leading whitespace is insignificant.
   (=skip-whitespace
    (=or (=section)
         (=object)
         (=listing)
         (=paragraph)))))

(defun =document ()
  (=prog1 (=contents)
          (=or (=end-of-document)
               ;; Unless all input was successfully parsed something went
               ;; wrong.
               (=syntax-error 'unrecognized-input))))
