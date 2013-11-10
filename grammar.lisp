;;;; MPC grammar used by GENEVA.MK2.

(in-package :geneva.mk2)

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
          (text (=or (=plain-text (=token end)) (=result "")))
          (_ (=token end)))
    (=result (funcall constructor text))))

(defun =markup ()
  (=or (=markup% #'make-bold
                 *bold-directive*)
       (=markup% #'make-italic
                 *italic-directive*)
       (=markup% #'make-fixed-width
                 *fixed-width-directive-start*
                 *fixed-width-directive-end*)
       (=markup% #'make-url
                 *url-directive-start*
                 *url-directive-end*)))

(defun =markup-directive ()
  (=one-of *markup-directives*))

(defun =text (until)
  (=zero-or-more
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
  (=let* ((text (=text (=or (=content-delimiter)
                            (=token *section-end*))))
          (_ (=content-delimiter)))
    (if text
        (=result (make-paragraph text))
        (=fail))))

(defun =list-item ()
  (=prog2 (=token *listing-item*)
          (=text (=or (=token *listing-item*)
                      (=content-delimiter)))))

(defun =listing ()
  (=let* ((items (=one-or-more (=list-item)))
          (_ (=content-delimiter)))
    (=result (make-listing items))))

(defun =object% (keyword constructor parser)
  (let ((delim (=token *object-delimeter*)))
    ;; DELIM KEYWORD TEXT DELIM PARSER.
    (=let* ((_ (=and delim (=string keyword nil)))
            (description (=text (=or delim (=content-delimiter))))
            (_ (=or (=and delim (=newline*))
                    ;; Description is not terminated properly
                    (=syntax-error 'malformed-object)))
            (body parser))
      (=result (funcall constructor description body)))))

(defun =skip-horizontal-space (parser)
  "Skip horizontal whitespace and apply PARSER."
  (=and (=zero-or-more (=unless (=newline) (=whitespace)))
        parser))

(defun =url ()
  "We are really liberal as to whats a valid URL. That decision is
outside of MK2's scope. We even allow multiline strings with escaped
newlines."
  (=prog1 (=skip-horizontal-space (=string-of (=not (=token #\Newline))))
          (=or (=content-delimiter)
               ;; Object is not terminated properly
               (=syntax-error 'malformed-object))))

(defun =table-column ()
  (=and (=token *table-item*)
        (=text (=or (=token *table-item*)
                    (=newline*)))))

(defun =table-row ()
  (=one-or-more
   (=skip-horizontal-space (=table-column))))

(defun =table-body ()
  (=prog1 (=one-or-more (=prog1 (=table-row) (=newline*)))
          (=or (=content-delimiter)
               ;; Object is not terminated properly
               (=syntax-error 'malformed-object))))

(defun =code-terminator ()
  (=and (=skip-whitespace (=token *object-delimeter*))
        (=content-delimiter)))

(defun =code-line ()
  (=unless (=or (=code-terminator)
                (=end-of-input))
           (=line t)))

(defun =code-body ()
  (=let* ((lines (=zero-or-more (=code-line)))
          (_ (=or (=code-terminator)
                  (=syntax-error 'malformed-object))))
    (=result (apply #'concatenate 'string lines))))

(defun =object ()
  (=or (=object% *media-keyword*     #'make-media     (=url))
       (=object% *table-keyword*     #'make-table     (=table-body))
       (=object% *plaintext-keyword* #'make-plaintext (=code-body))))

(defun =section ()
  (=handler-case
   (=let* ((_ (=token *section-start*))
           (header (=text (=content-delimiter)))
           (_ (=content-delimiter))
           (contents (=contents))
           (_ (=or (=skip-whitespace (=token *section-end*))
                   ;; Sections must be closed aye.
                   (=syntax-error 'section-not-closed))))
     (=result (make-section header contents)))
   ;; We have an unclosed section so we signal where it opened.
   (section-not-closed () (=syntax-error 'open-section))))

(defun =contents ()
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
