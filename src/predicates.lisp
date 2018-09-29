;;; A few predicates

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)


(defun lambda-p (exp)
  "Check whether the expression is a lambda"
  (and (< 1 (length exp))
       (symbolp (car exp))
       (or (cl-strings:starts-with (symbol-name (car exp)) "FN-")
           (eq (car exp) 'fn))))
