;;;; Tokens used in MK10 document syntax.

(defpackage mk10.tokens
  (:documentation "Tokens used by MK10.")
  (:use :cl)
  (:export :*section-start*
           :*section-end*
           :*listing-item*
           :*table-item*    
           :*object-delimeter*
           :*bold-directive*    
           :*italic-directive*
           :*code-directive-start*
           :*code-directive-end* 
           :*url-directive-start*
           :*url-directive-end*
           :*escape-directive*
           :*special-tokens*
           :*markup-directives*
           :*table-keyword*
           :*media-keyword*
           :*pre-keyword*))

(in-package :mk10.tokens)

;;; Syntax special characters
(defparameter *section-start*        #\< "Section start character.")
(defparameter *section-end*          #\> "Section end character.")
(defparameter *listing-item*         #\+ "Listing item character.")
(defparameter *table-item*           #\| "Table item character.")
(defparameter *object-delimeter*     #\# "Object delimeter character.")
(defparameter *bold-directive*       #\* "Bold directive character.")
(defparameter *italic-directive*     #\_ "Bold directive character.")
(defparameter *code-directive-start* #\{ "Code directive start character.")
(defparameter *code-directive-end*   #\} "Code directive end character.")
(defparameter *url-directive-start*  #\[ "Url directive start character.")
(defparameter *url-directive-end*    #\] "Url directive end character.")
(defparameter *escape-directive*     #\\ "Escape directive character.")

(defparameter *special-tokens* (list *section-start*
				     *section-end*
				     *listing-item*
				     *table-item*
				     *object-delimeter*
				     *escape-directive*)
  "Special tokens.")

(defparameter *markup-directives* (list *bold-directive*
					*italic-directive*
					*code-directive-start*
					*code-directive-end*
					*url-directive-start*
					*url-directive-end*)
  "Markup directives.")


;;; Syntax keywords
(defparameter *table-keyword* "TABLE" "Table tag word.")
(defparameter *media-keyword* "MEDIA" "Media tag word.")
(defparameter *pre-keyword*   "CODE"  "Preformatted text tag word.")

