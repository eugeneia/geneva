;;;; LPC grammar used by MK10.SERIALZE.

(in-package :mk10.serialize)

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
        (=plain-text (=or (=markup-directive)
                          until)))))

(defun =newline* ()
  (=and (=zero-or-more (=unless (=newline) (=whitespace)))
        (=newline)))

(defun =double-newline ()
  (=and (=newline*)
        (=newline*)))

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

(defun =object% (keyword constructor parser)
  (let ((delim (=token *object-delimeter*)))
    (=let* ((_ (=and delim (=string keyword nil) delim))
            (description (=text (=or delim
                                     (=content-delimiter))))
            (_ (=or delim
                    ;; Description is not terminated properly
                    (=syntax-error 'malformed-object)))
            (body parser))
      (=result (funcall constructor description body)))))

(defun =url ()
  "We are really liberal as to whats a valid URL. That decision is
outside of MK10's scope. We even allow multiline strings with escaped
newlines."
  (=prog1 (=string-of (=not (=token #\Newline)))
          (=or (=content-delimiter)
               ;; Object is not terminated properly
               (=syntax-error 'malformed-object))))

(defun =table-column ()
  (=and (=skip-whitespace (=token *table-item*))
        (=text (=newline*))))

(defun =table-row ()
  (=prog1 (=one-or-more (=table-column))
          (=newline*)))

(defun =table-body ()
  (=prog1 (=one-or-more (=table-row))
          (=or (=newline*)
               (=content-delimiter)
               ;; Object is not terminated properly
               (=syntax-error 'malformed-object))))

(defun =code-terminator ()
  (=and (=token *object-delimeter*)
        (=content-delimiter)))

(defun =code-line ()
  (=unless (=code-terminator)
           (=line t)))

(defun =code-body ()
  (=let* ((lines (=zero-or-more (=code-line)))
          (_ (=or (=code-terminator)
                  (=syntax-error 'malformed-object))))
    (=result (apply #'concatenate 'string lines))))

(defun =object ()
  (=or (=object% *media-keyword* #'make-media (=url))
       (=object% *table-keyword* #'make-table (=table-body))
       (=object% *pre-keyword*   #'make-pre   (=code-body))))

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
  (=prog1
   (=contents)
   (=or (=end-of-document)
        ;; Unless all input was successfully parsed something went wrong.
        (=syntax-error 'unrecognized-input))))
