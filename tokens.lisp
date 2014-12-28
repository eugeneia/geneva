;;;; Tokens used in MK10 document syntax.

(in-package :geneva.mk2.tokens)

;;; Syntax tokens
(defparameter *section-start*
  #\< "Section start character.")
(defparameter *section-end*
  #\> "Section end character.")
(defparameter *listing-item*
  #\+ "Listing item character.")
(defparameter *table-item*
  #\| "Table item character.")
(defparameter *object-delimiter*
  #\# "Object delimiter character.")
(defparameter *bold-directive*
  #\* "Bold directive character.")
(defparameter *italic-directive*
  #\_ "Bold directive character.")
(defparameter *fixed-width-directive-start*
  #\{ "Fixed-Width directive start character.")
(defparameter *fixed-width-directive-end*
  #\} "Fixed-Width directive end character.")
(defparameter *url-directive-start*
  #\[ "URL directive start character.")
(defparameter *url-directive-end*
  #\] "URL directive end character.")
(defparameter *url-url-start*
  #\( "URL url start character.")
(defparameter *url-url-end*
  #\) "URL url end character.")
(defparameter *break-directive*
  #\$ "Break directive character.")
(defparameter *escape-directive*
  #\\ "Escape directive character.")

(defparameter *special-tokens* (list *section-start*
				     *section-end*
				     *listing-item*
				     *table-item*
                                     *break-directive*
				     *object-delimiter*
				     *escape-directive*)
  "Special tokens.")

(defparameter *markup-directives* (list *bold-directive*
					*italic-directive*
					*fixed-width-directive-start*
					*fixed-width-directive-end*
					*url-directive-start*
					*url-directive-end*
                                        *url-url-start*
                                        *url-url-end*
                                        *break-directive*)
  "Markup directives.")


;;; Syntax keywords
(defparameter *table-keyword* "TABLE" "Table tag word.")
(defparameter *media-keyword* "MEDIA" "Media tag word.")
(defparameter *plaintext-keyword* "CODE" "Plaintext tag word.")
