;;;; Packages for the MK2 markup language.

(defpackage mk2.tokens
  (:documentation "Tokens used by the MK2 markup language.")
  (:use :cl)
  (:export :*section-start*
           :*section-end*
           :*listing-item*
           :*table-item*    
           :*object-delimeter*
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
   "Read and print Geneva documents using the MK2 markup language..")
  (:use :cl
        :geneva
        :mk2.tokens
        :mpc
        :mpc.characters
        :pretty-string
        :split-sequence)
  (:export :read-mk2
           :syntax-error
           :line-position
           :character-position
           :print-mk2))
