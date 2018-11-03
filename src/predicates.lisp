;;; A few predicates

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defun lambda-p (exp)
  "Check whether the expression is a lambda"
  (and (< 1 (length exp))
       (symbolp (car exp))
       (or (cl-strings:starts-with (symbol-name (car exp)) "FN-")
           (eq (car exp) 'fn))))

(defun id-p (exp)
  (or (symbolp exp)
      (match exp ((list 'cons (cons 'quote _) _) t))))


(defun kid-p (exp)
  "Tell if the thing is id and starts with `:'"
  (or (keywordp exp)
      (match exp ((list 'cond (cons 'quote _) _) t))))
