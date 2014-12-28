;;;; Normalize text for Geneva.

(in-package :geneva)

(defun join-strings (text)
  "Join :PLAIN text tokens in TEXT."
  (reduce (lambda (text item)
            (let ((last-item (first (last text))))
              (if (and text
                       (eq (content-type item) :plain)
                       (eq (content-type last-item) :plain))
                  `(,@(butlast text)
                    ,(concatenate 'string last-item item))
                  (append text (list item)))))
          (cons nil text)))

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "Characters considered whitespace.")

(defun normalize-whitespace (string &key trim)
  "Normalize *WHITESPACE* in STRING and optionally TRIM :LEFT,
:RIGHT or :BOTH."
  (if (string= string "") ""
      (let* ((words (split-sequence-if
                     (lambda (char)
                       (member char *whitespace*))
                     string))
             (stripped (remove "" words :test #'equal)))
        (if stripped
            (format nil "~:[ ~;~]~{~a~^ ~}~:[ ~;~]"
                    (or (null stripped)
                        (eq :left trim)
                        (eq :both trim)
                        (< 0 (length (first words))))
                    stripped
                    (or (null stripped)
                        (eq :right trim)
                        (eq :both trim)
                        (< 0 (length (first (last words))))))
            ;; STRING was only whitespace.
            " "))))

(defun normalize-text-item (item &key trim)
  "Normalize *WHITESPACE* in text ITEM and optionally TRIM :LEFT or
:RIGHT."
  (ecase #1=(content-type item)
    (:plain
     (normalize-whitespace #2=(content-values item) :trim trim))
    ((:bold :italic :fixed-width)
     (list #1# (normalize-whitespace #2# :trim :both)))
    (:url
     (multiple-value-bind (string url) #2#
       `(,#1# ,(normalize-whitespace string :trim :both)
              ,@(when url
                  `(,(normalize-whitespace url :trim :both))))))
    (:break item)))

(defun position-non-whitespace-item (text &optional from-end)
  "Get position of first non-whitespace item in TEXT and maybe start
FROM-END."
  (labels ((whitespace-p (c) (member c *whitespace*))
           (whitespace-item-p (item)
             (unless (eq item :break)
               (not (find-if-not #'whitespace-p
                                 (content-values item))))))
    (position-if-not #'whitespace-item-p text :from-end from-end)))

(defun trim-whitespace-items (text)
  "Remove whitespace from start and end of TEXT."
  (let ((start (position-non-whitespace-item text)))
    (when start
      (subseq text start (1+ (position-non-whitespace-item text t))))))

(defun normalize-text-whitespace (text)
  "Remove obsoltete whitespace from TEXT."
  (case (length text)
    ;; NIL → NIL.
    (0 nil)
    ;; (X) → Trim X both.
    (1 `(,(normalize-text-item (first text) :trim :both)))
    ;; (X Y) → Trim X left, trim Y right.
    (2 `(,(normalize-text-item (first text) :trim :left)
         ,(normalize-text-item (second text) :trim :right)))
    ;; (X1 .. Xn) → Trim X1 left, trim Xn right.
    (otherwise
     `(,(normalize-text-item (first text) :trim :left)
       ,@(loop for item in (butlast (rest text))
            collect (normalize-text-item item))
       ,(normalize-text-item (first (last text)) :trim :right)))))

(defun remove-empty-markup (text)
  "Remove empty markup from TEXT."
  (flet ((empty-p (string)
           (not (find-if-not (lambda (char)
                               (member char *whitespace*))
                             string))))
    (remove-if (lambda (text)
                 (and (listp text)
                      (empty-p (second text))))
               text)))

(defun normalize-text (text)
  "Remove empty markup and join adjacent strings in TEXT, then remove
superfluous whitespace."
  (loop for head = (split-sequence :break text) then (cdr head)
        for line = (car head)
     while head append
       (remove ""
               (normalize-text-whitespace
                (trim-whitespace-items
                 (join-strings
                  (remove-empty-markup line))))
               :test #'equal)
     when (cdr head) collect :break))

(defun trim-whitespace-suffixes (lines)
  "Trim whitespace suffixes from LINE."
  (loop for line in lines collect
       (string-right-trim *whitespace* line)))

(defun normalize-plaintext (string)
  "Remove leading and ending whitespace, global indent and whitespace
  line-suffixes from plaintext STRING ."
  (let* ((lines (trim-whitespace-suffixes
                 (trim-whitespace-items
                  (split-sequence #\Newline string))))
         (indent (loop for line in lines
                    for start = (position-if-not
                                 (lambda (char)
                                   (char= #\Space char))
                                 line)
                    when start minimize start))
         (unindented (mapcar (lambda (line)
                               (if (> (length line) 0)
                                   (subseq line indent)
                                   line))
                             lines)))
    (format nil "~{~a~^~%~}" unindented)))
