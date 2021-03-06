;;;; Tests for GENEVA.

(defpackage geneva.test
  (:use :cl
        :geneva)
  (:export :test-join-strings
           :test-normalize-whitespace
           :test-normalize-text-whitespace
           :test-normalize-text
           :test-normalize-plaintext))

(in-package :geneva.test)

(defun test-join-strings ()
  "Test JOIN-STRINGS."
  (assert (equal (geneva::join-strings
                  '("foo" " bar" (:bold "|") "baz " (:bold "|")
                    "boom" "yeah"))
                 '("foo bar" (:bold "|") "baz " (:bold "|")
                   "boomyeah"))))

(defun test-normalize-whitespace ()
  "Test NORMALIZE-WHITESPACE."
  (assert (string= (geneva::normalize-whitespace
                    "  foo  bar baz  ")
                    " foo bar baz "))
  (assert (string= (geneva::normalize-whitespace
                    "  foo  bar baz  "
                    :trim :left)
                    "foo bar baz "))
  (assert (string= (geneva::normalize-whitespace
                    "  foo  bar baz  "
                    :trim :right)
                    " foo bar baz"))
  (assert (string= (geneva::normalize-whitespace
                    "  foo  bar baz  "
                    :trim :both)
                    "foo bar baz"))
  (assert (string= (geneva::normalize-whitespace
                    "")
                   ""))
  (assert (string= (geneva::normalize-whitespace
                    "    ")
                   " ")))

(defun test-normalize-text-whitespace ()
  "Test NORMALIZE-TEXT-WHITESPACE."
  (assert (equal (geneva::normalize-text-whitespace
                  '(" Hello world  what "
                    (:bold "are you") "?  "))
                 '("Hello world what "
                   (:bold "are you") "?")))
  (assert (equal (geneva::normalize-text-whitespace
                  '())
                 '()))
  (assert (equal (geneva::normalize-text-whitespace
                  '(""))
                 '("")))
  (assert (equal (geneva::normalize-text-whitespace
                  '(" Hello "))
                 '("Hello")))
  (assert (equal (geneva::normalize-text-whitespace
                  '(" Hello you "))
                 '("Hello you"))))

(defun test-normalize-text ()
  "Test NORMALIZE-TEXT."
  (assert (equal (geneva::normalize-text
                  '((:italic "") " Hello world " " what "
                    (:bold "are you") "?  "))
                 '("Hello world what "
                   (:bold "are you") "?")))
  (assert (equal (geneva::normalize-text
                  '(" " (:italic "") " Hello world " " what "
                    (:bold "are you") " "))
                 '("Hello world what "
                   (:bold "are you"))))
  (assert (equal (geneva::normalize-text
                  '("" (:italic " ") " f " (:bold " ") ""))
                 '("f"))))

(defun test-normalize-plaintext ()
  "Test NORMALIZE-PLAINTEXT."
  (assert (string= (geneva::normalize-plaintext
                    "   foo

    bar
  boom
   baz")
                   " foo

  bar
boom
 baz")))
