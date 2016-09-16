;;;; MaxPC grammar used by GENEVA.MK2.

(in-package :geneva.mk2)

(defun unescape (str)
  "Unescape backslash escaped STR."
  (flet ((next (s) (position #\\ str :start s)))
    (with-output-to-string (out)
      (loop with l = (length str)
         for s = 0 then (1+ p)
         for p = (next s) then (when (> l s) (next (1+ s)))
       do (write-string (subseq str s p) out)
       while p))))

(defun ?escape ()
  (?char *escape-directive*))

(defun ?escaped-char ()
  (?seq (?escape) (?not (?end))))

(defun ?plain-char ()
  (%or (?escaped-char) (?not (?end))))

(defun =plain-text (until)
  (=transform (=subseq (%some (%diff (?plain-char) until)))
              'unescape))

(defun ?token (char)
  (%diff (?char char) (?escaped-char)))

(defun ?newline* ()
  (?seq (%any (%diff (?whitespace) (?newline)))
        (?newline)))

(defun ?double-newline ()
  (?seq (?newline*) (?newline*)))

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
  (?seq (%any (?whitespace)) (?end)))

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
  (=destructure (_ _ description _ body)
      (=list delimiter
             (?string keyword nil)
             (=text (%or delimiter (?content-delimiter)))
             (%or (?seq delimiter (?newline*))
                  ;; Description is not terminated properly
                  (?syntax-error 'malformed-element))
             parser)
    (funcall constructor description body)))

(defun ?horizontal-whitespace ()
  (%diff (?whitespace) (?newline)))

(defun =url ()
  "We are liberal as to whats a valid URL. That decision is out of scope. We
even allow multiline strings with escaped newlines."
  (=destructure (_ url _)
      (=list (%any (?horizontal-whitespace))
             (=transform (=subseq (%any (?not (?token #\Newline)))) 'unescape)
             (%or (?content-delimiter)
                  ;; Object is not terminated properly
                  (?syntax-error 'malformed-element)))))

(defun =table-column ()
  (=destructure (_ text)
      (=list (?token *table-item*)
             (=text (%or (?token *table-item*)
                         (?newline*))))))

(defun =table-row ()
  (=destructure (_ row _)
      (=list (%any (?horizontal-whitespace))
             (%some (=table-column))
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
  (?seq (%any (?whitespace))
        (?token *object-delimiter*)
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
                            (%or (?seq (%any (?whitespace))
                                       (?token *section-end*))
                                 ;; Sections must be closed aye.
                                 (?syntax-error 'section-not-closed)))
                   (make-section header contents))
    ;; We have an unclosed section so we signal where it opened.
    (section-not-closed () (?syntax-error 'open-section))))

(defun =contents ()
  (%any (=destructure (_ element)
            (=list (%any (?whitespace)) ; Leading whitespace is insignificant.
                   (%or '=section/p
                        (=object)
                        (=listing)
                        (=paragraph))))))

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
