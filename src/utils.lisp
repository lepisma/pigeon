(in-package #:pigeon)


(defun indent-string (text &key indent)
  (let ((pad (make-string indent :initial-element #\space)))
    (cl-strings:join
     (mapcar (cut format nil "~A~A" pad <>) (cl-strings:split text #\linefeed))
     :separator (string #\linefeed))))

(defun kebab-to-snake (text)
  (cl-strings:replace-all text "-" "_"))
