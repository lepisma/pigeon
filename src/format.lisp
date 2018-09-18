;;; Functions for formatting the forms

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defparameter *indent* 4
  "Indent for the generated python code. We use spaces around here.")

(defparameter *infix-ops* '(+ - * / % **)
  "Infix operators in python")

(defun fmt-atom (exp)
  (cond ((stringp exp) #?"\"${exp}\"")
        ((eq 't exp) "True")
        ((eq 'f exp) "False")
        ((eq 'none exp) "None")
        ((symbolp exp) (fmt-id exp))
        ((numberp exp) (format nil "~A" exp))
        ((vectorp exp) (fmt-vector exp))))

(defun fmt-id (exp)
  (string-downcase (kebab-to-snake (symbol-name exp))))

(defun fmt-lambda-list (args)
  (cl-strings:join (mapcar #'fmt args) :separator ", "))

(defun fmt-block (body)
  (indent-string
   (cl-strings:join (mapcar #'fmt body) :separator (string #\linefeed))
   :indent *indent*))

(defun fmt-fn (name lambda-list body)
  (let ((lambda-list (fmt-lambda-list lambda-list))
        (body (fmt-block body)))
    #?"def ${(fmt-id name)}(${lambda-list}):\n${body}"))

(defun fmt-call-infix (fn args)
  (cl-strings:join (mapcar #'fmt args) :separator #?" ${(fmt fn)} "))

(defun fmt-call (fn args)
  (cond
    ((member fn *infix-ops*) (fmt-call-infix fn args))
    (t #?"${(fmt-id fn)}(${(fmt-lambda-list args)})")))

(defun fmt-setf (lhs rhs)
  #?"${(fmt lhs)} = ${(fmt rhs)}")

(defun fmt-list (args)
  (let ((items (mapcar #'fmt args)))
    #?"[${(cl-strings:join items :separator ", ")}]"))

(defun fmt-tuple (args)
  (let* ((items (mapcar #'fmt args))
         (items (if (= 1 (length items)) (append items '("")) items)))
    #?"(${(cl-strings:join items :separator ", ")})"))

(defun fmt-vector (vec)
  (let ((list (map 'list #'identity vec)))
    #?"np.array(${(fmt-list list)})"))

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
    ((guard x (atom x))
     (fmt-atom x))
    ((cons fn args)
     (fmt-call fn args))))
