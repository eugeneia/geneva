;;;; Test GENEVA.MK2's READ-MK2 and PRINT-MK2.

(defpackage geneva.mk2-test
  (:use :cl
        :mpc
        :geneva
        :geneva.mk2)
  (:export :test-paragraph
           :test-listing
           :test-table
           :test-mk2))

(in-package :geneva.mk2-test)

(defparameter *character-range* 256
  "Range for random character values.")

(defparameter *test-iterations* 32
  "Number of test iterations.")

(defmacro one-of (&body body)
  "Evaluate a randomly chosen BODY form."
  (let ((length (length body)))
    `(case (random ,length)
       ,@(loop for i from 0 to length
               for form in body
            collect `(,i ,form)))))

(defun random-string (length)
  "Return a random string of LENGTH."
  (let ((string (make-string length)))
    (loop for i from 0 to (1- length)
       do (setf (aref string i) (one-of #\x #\Space)))
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
            (make-url random-string))))

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
                  ;; 50/50 chance of possible subsection.
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

(defun random-document ()
  "Generate a random document."
  (collect-n (random*) (random-element)))

(defun mk2-parser (parser)
  "Return parser for PARSER function."
  (lambda (in)
    (run parser in)))

(defun test-integrity (object parser)
  "Perform integrity test for OBJECT, printed and subsequently parsed by
PARSER."
  (let* ((mk2 (with-output-to-string (*standard-output*)
                (geneva.mk2::print-content object)))
         (read-object (funcall parser mk2)))
    (unless (equal object read-object)
      (format t "FAIL:~%~a~:S~%~:S~%~%" mk2 object read-object))))

(defun test-paragraph ()
  "Test MK2 paragraph printing and parsing."
  (test-integrity (random-paragraph)
                  (mk2-parser (geneva.mk2::=paragraph))))

(defun test-listing ()
  "Test MK2 listing printing and parsing."
  (test-integrity (random-listing)
                  (mk2-parser (geneva.mk2::=listing))))

(defun test-table ()
  "Test MK2 table printing and parsing."
  (test-integrity (random-table)
                  (mk2-parser (geneva.mk2::=object))))

(defun test-mk2 (&optional (tests '(test-paragraph
                                    test-listing)))
  "Run TESTS."
  (loop for test in tests do
       (format t "~A~%" test)
       (dotimes (i *test-iterations*) 
         (funcall (symbol-function test)))))
