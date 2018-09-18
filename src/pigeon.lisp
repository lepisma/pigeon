(in-package #:pigeon)

(defun transform (input-text)
  (fmt (read-from-string input-text)))

(defun transform-file (input-path output-path)
  (let ((code (transform (uiop:read-file-string input-path))))
    (with-open-file (fp output-path
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
      (format fp code))))
