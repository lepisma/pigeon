(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defun prepend-pad (lines pad)
  (mapcar (cut format nil "~A~A" pad <>) lines))

(defun indent-string (text &key indent)
  (let ((pad (make-string indent :initial-element #\space)))
    (cl-strings:join
     (prepend-pad (cl-strings:split text #\linefeed) pad)
     :separator (string #\linefeed))))

(defun kebab-to-snake (text)
  (cl-strings:replace-all text "-" "_"))
