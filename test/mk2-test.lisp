;;;; Test GENEVA.MK2's READ-MK2 and PRINT-MK2.

(defpackage geneva.mk2-test
  (:use :cl
        :mpc
        :geneva
        :geneva.mk2
        :geneva.mk2.tokens)
  (:export :random-paragraph
           :random-listing
           :random-table
           :random-media
           :random-plaintext
           :random-section
           :test-mk2))

(in-package :geneva.mk2-test)

(defparameter *test-iterations* 8
  "Number of test iterations. Due to sections, this variable will
increase the runtime of TEST-MK2 exponentially.")

(defmacro one-of (&body body)
  "Evaluate a randomly chosen BODY form."
  (let ((length (length body)))
    `(case (random ,length)
       ,@(loop for i from 0 to length
               for form in body
            collect `(,i ,form)))))

(defun random-character ()
  (one-of #\x #\Space
          *section-start*
          *section-end*
          *listing-item*
          *table-item*
          *object-delimiter*
          *bold-directive*
          *italic-directive*
          *fixed-width-directive-start*
          *fixed-width-directive-end*
          *url-directive-start*
          *url-directive-end*
          *escape-directive*))

(defun random-string (length)
  "Return a random string of LENGTH."
  (let ((string (make-string length)))
    (loop for i from 0 to (1- length)
       do (setf (aref string i) (random-character)))
    string))

(defun random* ()
  "Return a random number ranging from zero to *TEST-ITERATIONS*."
  (random *test-iterations*))

(defmacro collect-n (n &body forms)
  "Evaluate and collect the result of FORMS (implicit PROGN) N times."
  (let ((i-sym (gensym "counter")))
    `(loop for ,i-sym from 0 to ,n collect (progn ,@forms))))

(defun random-text-token ()
  "Generate random text token."
  (let ((random-string (random-string (1+ (random*)))))
    (one-of random-string
            (make-bold random-string)
            (make-italic random-string)
            (make-fixed-width random-string)
            (make-url random-string)
            (make-url random-string random-string))))

(defun random-text ()
  "Generate random text."
  (collect-n (random*)
    (random-text-token)))

(defun random-paragraph ()
  "Generate random paragraph."
  (make-paragraph (list* (random-text-token) (random-text))))

(defun random-listing ()
  "Generate random listing."
  (make-listing (collect-n (random*) (random-text))))

(defun random-table ()
  "Generate random table."
  (let ((columns (1+ (random 4))))
    (make-table (random-text)
                (collect-n (1+ (random 4))
                  (collect-n columns (random-text))))))

(defun random-media ()
  "Generate a random media element."
  (make-media (random-text) "xxxxx"))

(defun random-plaintext ()
  "Generate random plaintext element."
  (make-plaintext (random-text) (random-string (random*))))

(defun random-section ()
  "Generate random section."
  (make-section (random-text)
                (collect-n (random*)
                  ;; 50% chance of possible subsection.
                  (one-of (one-of (random-paragraph)
                                  (random-listing)
                                  (random-table)
                                  (random-media)
                                  (random-plaintext))
                          ;; With possible subsection.
                          (one-of (random-element))))))

(defun random-element ()
  "Generate random element."
  (one-of (random-paragraph)
          (random-listing)
          (random-table)
          (random-media)
          (random-plaintext)
          (random-section)))

(defun random-elements ()
  "Generate a list of random elements."
  (collect-n (random*) (random-element)))

(defun test-integrity (elements)
  "Perform integrity test for ELEMENTS, printed and subsequently parsed."
  (let* ((document (make-document elements))
         (mk2 (with-output-to-string (*standard-output*)
                (print-mk2 document)))
         read-document)
    (handler-case
        (progn (setf read-document (read-mk2 mk2))
               (assert (equal document read-document)))
      (error (error)
        (format t "FAIL: ~a ~S~%~a~:S~%~:S~%~%"
                error error mk2 document read-document)))))

(defun test-mk2 (&rest tests)
  "Run TESTS."
  (if tests
      (loop for test in tests do
           (format t "~A~%" test)
           (dotimes (i *test-iterations*)
             (test-integrity (list (funcall test)))))
      (progn
        (format t "DOCUMENT~%")
        (dotimes (i *test-iterations*)
          (test-integrity (random-elements))))))
