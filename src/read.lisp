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

(defun read-until (stream okay-cond)
  (map 'string #'identity
       (loop for c = (peek-char nil stream nil nil t)
             while (and c (funcall okay-cond c))
             do (read-char stream nil nil t)
             collect c)))

(defun read-open-close (stream open-char close-char)
  "Read a nest string following open and close chars."
  (let ((nesting 0))
    (map 'string #'identity
         (loop for c = (peek-char nil stream nil nil t)
               while (and c (or (char-not-equal close-char c) (not (zerop nesting))))
               do (progn
                    (read-char stream nil nil t)
                    (cond ((char-equal open-char c) (incf nesting))
                          ((char-equal close-char c) (decf nesting))))
               collect c))))

(defun read-case-sensitive-id (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((name (read-until stream (lambda (c)
                                   (or (alphanumericp c)
                                       (char-equal #\_ c))))))
    `(cons 'id ,name)))

(defun read-pigeon-list (stream char)
  (declare (ignore char))
  (let ((list-stuff (read-open-close stream #\[ #\])))
    (read-char stream nil nil t) ;; discarding the last #\]
    (read-from-string #?"(pg-list ${list-stuff})")))

(defun read-pigeon-list-comp (stream subchar arg)
  (declare (ignore arg))
  (ematch (read-pigeon-list stream subchar)
    ((list 'pg-list thing :for item :in collection)
     `(pg-list-comp ,thing ,item ,collection))
    ((list 'pg-list thing :for item :in collection :if condition)
     `(pg-list-comp ,thing ,item ,collection ,condition))))

(defun enable-case-sensitive-id-syntax ()
  (set-dispatch-macro-character #\# #\i #'read-case-sensitive-id))

(defun enable-pigeon-list-syntax ()
  (set-macro-character #\[ #'read-pigeon-list))

(defun enable-pigeon-list-comp-syntax ()
  (make-dispatch-macro-character #\@)
  (set-dispatch-macro-character #\@ #\[ #'read-pigeon-list-comp))

(defun read-pg (input-path)
  "Read pigeon code forms"
  (enable-case-sensitive-id-syntax)
  (with-open-file (fp input-path)
    (loop for form = (read fp nil)
          while form collect form)))
