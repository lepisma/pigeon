;;; Functions for formatting the forms

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defparameter *indent* 4
  "Indent for the generated python code. We use spaces around here.")

(defparameter *infix-ops* '(+ - * / % **)
  "Infix operators in python")

(defun fmt-atom (exp)
  (cond ((stringp exp) #?""${exp}"")
        ((eq 't exp) "True")
        ((eq 'f exp) "False")
        ((eq 'none exp) "None")
        ((symbolp exp) (fmt-id exp))))

(defun fmt-id (exp)
  (string-downcase (kebab-to-snake (symbol-name exp))))

(defun fmt-lambda-list (args)
  (cl-strings:join (mapcar #'fmt args) :separator ", "))

(defun fmt-fn (name lambda-list body &key level)
  (let ((lambda-list (fmt-lambda-list lambda-list))
        (body (fmt body :level (+ 1 level))))
     #?"def ${(fmt-id name)}(${lambda-list})\n${body}"))

(defun fmt-call-infix (fn args)
  (cl-strings:join (mapcar #'fmt args) :separator #?" ${(fmt fn)} "))

(defun fmt-call (fn args)
  (cond
    ((member fn *infix-ops*) (fmt-call-infix fn args))
    (t #?"${(fmt-id fn)}(${(fmt-lambda-list args)})")))

(defun fmt (exp &key (level 0))
  (indent-string
   (ematch exp
     ((list 'def name lambda-list body)
      (fmt-fn name lambda-list body :level level))
     ((cons fn args)
      (fmt-call fn args))
     ((guard x (atom x))
      (fmt-atom exp)))
   :indent (* level *indent*)))

;;; Here is a program I would like to transform to python

(fmt
 '(def hello-world (a b c)
   (print (+ "hello" a b c))))

;;; The output should be this
;; "def hello_world(a, b, c)
;;     print(\"hello\" + a + b + c)"
