(in-package #:pigeon)

(defun load-pgl (input-path)
  "Load a pgl file with macro definitions."
  (with-open-file (fp input-path)
    (loop for form = (read fp nil)
          while form
          do (match form
               ((list* 'defmacro name _)
                (eval form)
                (setf *macros* (cons name *macros*)))
               (form (eval form))))))

(defun read-code (input-path)
  (with-open-file (fp input-path)
    (let ((forms (loop for form = (read fp nil)
                       while form collect form)))
      (cl-strings:join (mapcar #'fmt forms)
                       :separator (make-string 2 :initial-element #\linefeed)))))

(defun write-code (code output-path)
  (with-open-file (fp output-path
                      :direction :output
                      :if-exists :overwrite
                      :if-does-not-exist :create)
    (format fp code)))

(defun transform-file (input-path output-path)
  (write-code (read-code input-path) output-path))
