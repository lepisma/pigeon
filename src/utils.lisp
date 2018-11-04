(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defun prepend-pad (lines pad-str)
  (mapcar (cut format nil "~A~A" pad-str <>) lines))

(defun get-line-pad (line pad-char &optional (at 0))
  (if (or (= at (length line)) (char-not-equal (elt line at) pad-char))
      at
      (get-line-pad line pad-char (+ 1 at))))

(defun get-min-pad (lines pad-char)
  (let ((lines (remove-if (cut string-equal "" <>) lines)))
    (if (null lines) 0 (apply #'min (mapcar (cut get-line-pad <> pad-char) lines)))))

(defun indent-string (text &key indent)
  (let ((pad (make-string indent :initial-element #\space)))
    (cl-strings:join
     (prepend-pad (cl-strings:split text #\linefeed) pad)
     :separator (string #\linefeed))))

(defun dedent-string (text)
  "Dedent common whitespace from the text."
  (let* ((lines (cl-strings:split text #\linefeed))
         (n-pad (get-min-pad lines #\ )))
    (cl-strings:join (mapcar (cut subseq <> n-pad) lines) :separator #?"\n")))

(defun kebab-to-snake (text)
  (cl-strings:replace-all text "-" "_"))
