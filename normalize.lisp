;;;; Normalize text for Geneva.

(in-package :geneva)

(defun join-strings (text)
  "Join strings in TEXT."
  (reduce (lambda (text token)
            (let ((last-token (first (last text))))
              (if (and (stringp token)
                       (stringp last-token))
                  `(,@(butlast text)
                    ,(concatenate 'string last-token token))
                  (append text (list token)))))
          (cons nil text)))
#| Test: (equal (join-strings
                  '("foo" " bar" nil "baz " nil "boom" "yeah"))
                '("foo bar" nil "baz " nil "boomyeah")) |#

(defparameter *whitespace* '(#\Newline #\Return #\Space)
  "Whitespace characters we do consider.")

(defun trim-whitespace (string)
  "Trim *WHITESPACE* from STRING."
  (string-trim *whitespace* string))

(defun normalize-whitespace (string &key trim)
  "Normalize whitespace (space, carriage return and newline) in STRING
  and optionally TRIM :LEFT or  :RIGHT."
  (let ((words (split-sequence-if
                (lambda (char)
                  (member char *whitespace*))
                string)))
    (format nil "~@[~a~]~{~a~^ ~}~@[~a~]"
            (unless (or (eq :left trim)
                        (< 0 (length (first words))))
              " ")
            (remove "" words :test #'equal)
            (unless (or (eq :right trim)
                        (< 0 (length (first (last words)))))
              " "))))

(defun normalize-text (text)
  "Remove superfluous whitespace from TEXT."
  (let ((text (join-strings text)))
    `(,(normalize-whitespace (first text) :trim :left)
       ,@(loop for token in (butlast (rest text))
            if (stringp token)
            collect (normalize-whitespace token)
            else collect token)
       ,(normalize-whitespace (first (last text)) :trim :right))))
#| Test: (equal (normalize-text '(" Hello world " " what "
                                  (:b "are you") "?  "))
                                '("Hello world what "
                                  (:B "are you") "?")) |#

(defun normalize-plaintext (string)
  "Remove global indent from plaintext STRING."
  (let* ((lines (split-sequence #\Newline string ))
         (indent (- (loop for line in lines
                       for start = (position-if
                                    (lambda (char)
                                      (not (char= #\Space char)))
                                    line)
                       when start maximize start)
                    2)))
    (format nil "~{~a~%~}" (mapcar (lambda (line)
                                     (if (> (length line) 0)
                                         (subseq line indent)
                                         line))
                                   lines))))
#| Test: (string= (normalize-plaintext
"   foo

    bar
  boom
   baz")

" foo

  bar
boom
 baz
") |#
