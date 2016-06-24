;;;; MPC grammar used by GENEVA.MK2.

(in-package :geneva.mk2)

(defun ?escape ()
  (?char *escape-directive*))

(defun ?escaped-char ()
  (?list (?escape) (?not (?end))))

(defun ?token (char)
  (%diff (?char char) (?escaped-char)))

(defun =escaped-seq (until)
  (%or (=destructure (_ seq)
           (=list (?escape)
                  (=subseq (?list (?not (?end))
                                  (%any (%diff (?not (?escape)) until))))))
       (=subseq (%some (%diff (?not (?escape)) until)))))

(defun =plain-text (until)
  (=destructure (&rest items) (%some (=escaped-seq until))
    (apply 'concatenate 'string items)))

(defun ?newline* ()
  (?list (%any (%diff (?whitespace) (?newline)))
         (?newline)))

(defun ?double-newline ()
  (?list (?newline*) (?newline*)))

(defun =markup1 (constructor start &optional (end start))
  (=destructure (_ text _)
      (=list (?token start)
             (=plain-text (%or (?token end) (?double-newline)))
             (?token end))
    (funcall constructor text)))

(defun =url-text ()
  (=destructure (x y)
      (=list (=markup1 #'identity *url-directive-start* *url-directive-end*)
             (%maybe (=markup1 #'identity #\( #\))))
    (make-url x y)))

(defun =markup ()
  (%or (=markup1 #'make-bold
                 *bold-directive*)
       (=markup1 #'make-italic
                 *italic-directive*)
       (=markup1 #'make-fixed-width
                 *fixed-width-directive-start*
                 *fixed-width-directive-end*)
       (=url-text)))

(defun ?markup-directive ()
  (?test ('member *markup-directives*)))

(defun =text-token (until)
  (%or (=markup)
       (=plain-text (%or (?markup-directive) until))
       ;; Ignore incomplete markup.
       (=subseq (?markup-directive))))

(defun =text (until)
  (%any (=text-token until)))

(defun ?end-of-document ()
  (%skip-whitespace (?end)))

(defun ?content-delimiter ()
  (%or (?double-newline) (?end-of-document)))

(defun =paragraph ()
  (=destructure (text _)
      (=list (%some (=text-token (%or (?content-delimiter)
                                      (?token *section-end*))))
             (?content-delimiter))
    (make-paragraph text)))

(defun =list-item ()
  (=destructure (_ text)
      (=list (?token *listing-item*)
             (=text (%or (?token *listing-item*)
                         (?content-delimiter))))))

(defun =listing ()
  (=destructure (items _)
      (=list (%some (=list-item))
             (?content-delimiter))
    (make-listing items)))

(defun =object1 (keyword constructor parser
                 &aux (delimiter (?token *object-delimiter*)))
  (=destructure (_ description _ body)
      (=list (?list delimiter (?string keyword nil))
             (=text (%or delimiter (?content-delimiter)))
             (%or (?list delimiter (?newline*))
                  ;; Description is not terminated properly
                  (?syntax-error 'malformed-element))
             parser)
    (funcall constructor description body)))

(defun %skip-horizontal-space (parser)
  "Skip horizontal whitespace and apply PARSER."
  (=destructure (_ result)
      (=list (%any (%diff (?whitespace) (?newline)))
             parser)))

(defun =url ()
  "We are liberal as to whats a valid URL. That decision is out of scope. We
even allow multiline strings with escaped newlines."
  (=destructure (url _)
      (=list (%skip-horizontal-space (=text (?token #\Newline)))
             (%or (?content-delimiter)
                  ;; Object is not terminated properly
                  (?syntax-error 'malformed-element)))))

(defun =table-column ()
  (=destructure (_ text)
      (=list (?token *table-item*)
             (=text (%or (?token *table-item*)
                         (?newline*))))))

(defun =table-row ()
  (=destructure (row _)
      (=list (%some (%skip-horizontal-space (=table-column)))
             (%maybe (?newline*)))))

(defun =table-body ()
  (=destructure (rows _)
      (=list (%some (=table-row))
             (%or (?content-delimiter)
                  ;; Single newline is ok in case last row ate one.
                  (?newline*)
                  ;; Object is not terminated properly
                  (?syntax-error 'malformed-element)))))

(defun ?plaintext-terminator ()
  (?list (%skip-whitespace (?token *object-delimiter*))
         (?content-delimiter)))

(defun =plaintext-line ()
  (%diff (=line) (%or (?plaintext-terminator) (?end-of-document))))

(defun =plaintext-body ()
  (=destructure (lines _)
      (=list (%any (=plaintext-line))
             (%or (?plaintext-terminator)
                  (?syntax-error 'malformed-element)))
    (format nil "~{~a~^~%~}" lines)))

(defun =object ()
  (%or
   (=object1 *media-keyword*     #'make-media     (=url))
   (=object1 *table-keyword*     #'make-table     (=table-body))
   (=object1 *plaintext-keyword* #'make-plaintext (=plaintext-body))))

(defun =section ()
  (%handler-case (=destructure (_ header _ contents _)
                     (=list (?token *section-start*)
                            (=text (?content-delimiter))
                            (?content-delimiter)
                            '=contents/p
                            (%or (%skip-whitespace (?token *section-end*))
                                 ;; Sections must be closed aye.
                                 (?syntax-error 'section-not-closed)))
                   (make-section header contents))
    ;; We have an unclosed section so we signal where it opened.
    (section-not-closed () (?syntax-error 'open-section))))

(defun =contents ()
  (%any
   ;; Leading whitespace is insignificant.
   (%skip-whitespace
    (%or '=section/p
         (=object)
         (=listing)
         (=paragraph)))))

;; We make the parsers of =SECTION and =CONTENTS callable by symbol. This is
;; necessary because they are mutually recursive.
(setf (fdefinition '=section/p) (=section)
      (fdefinition '=contents/p) (=contents))

(defun =document ()
  (=destructure (contents _)
      (=list
       '=contents/p
       (%or (?end-of-document)
            ;; Unless all input was successfully parsed something went wrong.
            (?syntax-error 'unrecognized-input)))))
