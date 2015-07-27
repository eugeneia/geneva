;;;; Package definitions for GENEVA-MK2.

(defpackage geneva.mk2.tokens
  (:documentation "Tokens used by the _Mk2_ markup language.")
  (:use :cl)
  (:export :*section-start*
           :*section-end*
           :*listing-item*
           :*table-item*    
           :*object-delimiter*
           :*bold-directive*    
           :*italic-directive*
           :*fixed-width-directive-start*
           :*fixed-width-directive-end* 
           :*url-directive-start*
           :*url-directive-end*
           :*escape-directive*
           :*special-tokens*
           :*markup-directives*
           :*table-keyword*
           :*media-keyword*
           :*plaintext-keyword*))

(defpackage geneva.mk2
  (:documentation
   "Implementation of _Mk2_¹, a plain text markup language for the Geneva
    document preparation system.

    + 1. _The Mk2 Markup Language_ [mk2.html]")
  (:use :cl
        :geneva
        :geneva.mk2.tokens
        :mpc
        :mpc.characters
        :split-sequence)
  (:import-from :geneva.utilities :wrap-string)
  (:export :read-mk2
           :syntax-error
           :open-section
           :malformed-element
           :unrecognized-input
           :line-position
           :character-position
           :print-mk2))
