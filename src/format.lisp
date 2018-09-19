;;; Functions for formatting the forms

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defparameter *indent* 4
  "Indent for the generated python code. We use spaces around here.")

(defparameter *infix-ops* '(+ - * / % **)
  "Infix operators in python")

(defun lambda-p (exp)
  "Check whether the expression is a lambda"
  (and (< 1 (length exp))
       (symbolp (car exp))
       (or (cl-strings:starts-with (symbol-name (car exp)) "FN-")
           (eq (car exp) 'fn))))

(defun lambda-parse-args (fn-form)
  (let ((args (subseq (symbol-name fn-form) 3)))
    (map 'list #'identity args)))

(defun fmt-lambda (fn-form body)
  (let ((args (unless (eq fn-form 'fn) (lambda-parse-args fn-form))))
    #?"(lambda ${(fmt-lambda-list args)}: ${(fmt body)})"))

(defun fmt-atom (exp)
  (cond ((characterp exp) (string-downcase (string exp)))
        ((stringp exp) #?"\"${exp}\"")
        ((eq 't exp) "True")
        ((eq 'f exp) "False")
        ((eq 'none exp) "None")
        ((symbolp exp) (fmt-id exp))
        ((numberp exp) (format nil "~A" exp))
        ((vectorp exp) (fmt-vector exp))))

(defun fmt-id (exp)
  (if (member exp *infix-ops*)
      (symbol-name exp)
      (string-downcase (kebab-to-snake (symbol-name exp)))))

(defun fmt-lambda-list (args)
  (cl-strings:join (mapcar #'fmt args) :separator ", "))

(defun fmt-block (body &optional no-indent)
  (indent-string
   (cl-strings:join (mapcar #'fmt body) :separator (string #\linefeed))
   :indent (if no-indent 0 *indent*)))

(defun fmt-fn (name lambda-list body)
  (let ((lambda-list (fmt-lambda-list lambda-list))
        (body (fmt-block body)))
    #?"def ${(fmt-id name)}(${lambda-list}):\n${body}"))

(defun fmt-call-infix (fn args)
  (let ((text (cl-strings:join (mapcar #'fmt args) :separator #?" ${(fmt fn)} ")))
    #?"(${text})"))

(defun fmt-call (fn args)
  (cond
    ((member fn *infix-ops*) (fmt-call-infix fn args))
    (t #?"${(fmt fn)}(${(fmt-lambda-list args)})")))

(defun fmt-setf (lhs rhs)
  #?"${(fmt lhs)} = ${(fmt rhs)}")

(defun fmt-list (args)
  (let ((items (mapcar #'fmt args)))
    #?"[${(cl-strings:join items :separator ", ")}]"))

(defun fmt-tuple (args)
  (let* ((items (mapcar #'fmt args))
         (items (if (= 1 (length items)) (append items '("")) items)))
    #?"(${(cl-strings:join items :separator ", ")})"))

(defun get-dict-items (args &optional earlier)
  (ematch args
    ((list* key value rest) (get-dict-items rest (cons (cons key value) earlier)))
    ((or (list garbage) nil) (reverse earlier))))

(defun fmt-dict-item (item)
  #?"${(fmt (car item))}: ${(fmt (cdr item))}")

(defun fmt-dict (args)
  (let ((items (get-dict-items args)))
    #?"{${(cl-strings:join (mapcar #'fmt-dict-item items) :separator ", ")}}"))

(defun fmt-vector (vec)
  (let ((list (map 'list #'identity vec)))
    #?"np.array(${(fmt-list list)})"))

(defun fmt-getf (key obj)
  #?"${(fmt obj)}[${(fmt key)}]")

(defun fmt-import-item (item)
  (ematch item
    ((list module :as alias)
     #?"import ${(fmt-id module)} as ${(fmt-id alias)}")
    (module #?"import ${(fmt-id module)}")))

(defun fmt-import (args)
  (let* ((from-p (member :from args))
         (item-args (if from-p (subseq args 0 (- (length args) 2)) args))
         (lines (mapcar #'fmt-import-item item-args)))
    (cl-strings:join (if from-p (prepend-pad lines #?"from ${(fmt-id (car (last args)))} ")
                         lines)
                     :separator (string #\linefeed))))

(defun fmt (exp)
  (ematch exp
    ((list* 'defun name lambda-list body)
     (fmt-fn name lambda-list body))
    ((list 'setf lhs rhs)
     (fmt-setf lhs rhs))
    ((cons 'list args)
     (fmt-list args))
    ((cons 'tuple args)
     (fmt-tuple args))
    ((cons 'dict args)
     (fmt-dict args))
    ((list 'getf key obj)
     (fmt-getf key obj))
    ((cons 'progn body)
     (fmt-block body t))
    ((cons 'import args)
     (fmt-import args))
    ((guard x (atom x))
     (fmt-atom x))
    ((guard x (lambda-p x))
     (apply #'fmt-lambda exp))
    ((cons fn args)
     (fmt-call fn args))))
