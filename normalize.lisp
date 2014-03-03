;;;; Normalize text for Geneva.

(in-package :geneva)

(defun join-strings (text)
  "Join strings in TEXT."
  (reduce (lambda (text item)
            (let ((last-item (first (last text))))
              (if (and (stringp item)
                       (stringp last-item))
                  `(,@(butlast text)
                    ,(concatenate 'string last-item item))
                  (append text (list item)))))
          (cons nil text)))

(defparameter *whitespace* '(#\Tab #\Newline #\Vt #\Ff #\Return #\Space)
  "Whitespace characters we do consider.")

(defun normalize-whitespace (string &key trim)
  "Normalize *WHITESPACE* in STRING and optionally TRIM :LEFT or
:RIGHT or :BOTH."
  (let* ((words (split-sequence-if
                 (lambda (char)
                   (member char *whitespace*))
                 string))
         (stripped (remove "" words :test #'equal)))
    (format nil "~:[ ~;~]~{~a~^ ~}~:[ ~;~]"
            (or (null stripped)
                (eq :left trim)
                (eq :both trim)
                (< 0 (length (first words))))
            stripped
            (or (null stripped)
                (eq :right trim)
                (eq :both trim)
                (< 0 (length (first (last words))))))))

(defun normalize-text-item (item &key trim)
  "Normalize *WHITESPACE* in text ITEM and optionally TRIM :LEFT or
:RIGHT."
  (if (stringp item)
      (normalize-whitespace item :trim trim)
      (list (first item)
            (normalize-whitespace (second item) :trim trim))))

(defun position-non-whitespace-item (text &optional from-end)
  "Get position of first non-whitespace item in TEXT and maybe start
FROM-END."
  (labels ((whitespace-p (c) (member c *whitespace*))
           (whitespace-item-p (item)
             (not (find-if-not #'whitespace-p
                               (if (stringp item)
                                   item
                                   (second item))))))
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
  (remove ""
          (normalize-text-whitespace
           (trim-whitespace-items
            (join-strings
             (remove-empty-markup text))))
          :test #'equal))

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
                    when start minimize start)))
    (format nil "~{~&~a~}" (mapcar (lambda (line)
                                     (if (> (length line) 0)
                                         (subseq line indent)
                                         line))
                                   lines))))

