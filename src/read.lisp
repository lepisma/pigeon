;;; Reading and loading stuff

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

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

(defun read-pg (input-path)
  "Read pigeon code forms"
  (with-open-file (fp input-path)
    (loop for form = (read fp nil)
          while form collect form)))
