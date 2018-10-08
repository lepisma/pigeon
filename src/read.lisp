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

(defun read-case-sensitive-id (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((name (map 'string #'identity
                   (loop for c = (peek-char nil stream nil nil t)
                         while (and c (or (alphanumericp c)
                                          (char-equal #\_ c)))
                         do (read-char stream nil nil t)
                         collect c))))
    `(cons 'id ,name)))

(defun enable-case-sensitive-id-syntax ()
  (set-dispatch-macro-character #\# #\i #'read-case-sensitive-id))

(defun read-pg (input-path)
  "Read pigeon code forms"
  (enable-case-sensitive-id-syntax)
  (with-open-file (fp input-path)
    (loop for form = (read fp nil)
          while form collect form)))
