;;; Functions for formatting the primitives that translate directly to python

(in-package #:pigeon)
(cl-interpol:enable-interpol-syntax)

(defparameter *indent* 4
  "Indent for the generated python code. We use spaces around here.")

(defparameter *infix-ops* '(+ - * / % ** == != > < >= <= >> << or and is in)
  "Infix operators in python")

(defparameter *statements* '(return not)
  "Statements that have direct call pattern in python.")

(defun lambda-parse-args (fn-form)
  (let ((args (subseq (symbol-name fn-form) 3)))
    (map 'list #'identity args)))

(defun add-return (body)
  "Add a return statement to the last item of the body"
  (append (subseq body 0 (- (length body) 1)) (list (cons 'return (last body)))))

(defun fmt-lambda (fn-form body)
  (let ((args (unless (eq fn-form 'fn) (lambda-parse-args fn-form))))
    #?"(lambda ${(fmt-lambda-list args)}: ${(fmt body)})"))

(defun fmt-string (string)
  "Need to check for multiline strings too."
  (if (find #\linefeed string)
      #?"\"\"\"${string}\"\"\""
      #?"\"${string}\""))

(defun fmt-atom (exp)
  (cond ((characterp exp) (string-downcase (string exp)))
        ((stringp exp) (fmt-string exp))
        ((eq 't exp) "True")
        ((eq 'f exp) "False")
        ((eq 'none exp) "None")
        ((null exp) "[]")
        ((symbolp exp) (fmt-id exp))
        ((numberp exp) (format nil "~A" exp))
        ((vectorp exp) (fmt-vector exp))))

(defun fmt-id (exp)
  (ematch exp
    ((list 'cons (cons 'quote _) id-string)
     id-string)
    ((guard x (member exp *infix-ops*))
     (symbol-name x))
    (_ (string-downcase (kebab-to-snake (symbol-name exp))))))

(defun fmt-lambda-list (args)
  (cl-strings:join (mapcar #'fmt args) :separator ", "))

(defun fmt-block (body &optional no-indent)
  (indent-string
   (cl-strings:join (mapcar #'fmt body) :separator (string #\linefeed))
   :indent (if no-indent 0 *indent*)))

(defun fmt-fn (name lambda-list body)
  (let ((lambda-list (fmt-lambda-list lambda-list))
        (body (fmt-block (add-return body))))
    #?"def ${(fmt-id name)}(${lambda-list}):\n${body}"))

(defun fmt-call-infix (fn args)
  (let ((text (cl-strings:join (mapcar #'fmt args) :separator #?" ${(fmt fn)} ")))
    #?"(${text})"))

(defun fmt-cond-clause (clause &optional first)
  (ematch clause
    ((cons t body)
     #?"else:\n${(fmt-block body)}")
    ((cons condition body)
     (cond (first #?"if ${(fmt condition)}:\n${(fmt-block body)}")
           (t #?"elif ${(fmt condition)}:\n${(fmt-block body)}")))))

(defun fmt-cond (clauses)
  (cl-strings:join (cons (fmt-cond-clause (car clauses) t)
                         (mapcar #'fmt-cond-clause (cdr clauses)))
                   :separator (string #\linefeed)))

(defun fmt-call (fn args)
  (cond
    ((member fn *infix-ops*) (fmt-call-infix fn args))
    ((member fn *macros*) (fmt (macroexpand-1 `(,fn ,@args))))
    ((member fn *statements*) #?"${(fmt fn)} ${(fmt-lambda-list args)}")
    ((eq fn 'cond) (fmt-cond args))
    (t #?"${(fmt fn)}(${(fmt-lambda-list args)})")))

(defun fmt-setf (lhs rhs)
  #?"${(fmt lhs)} = ${(fmt rhs)}")

(defun fmt-list-comp (thing item collection &optional condition)
  (let ((last-bit (if condition #?" if ${(fmt condition)}" "")))
    #?"[${(fmt thing)} for ${(fmt item)} in ${(fmt collection)}${last-bit}]"))

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

(defun fmt-ext (ext-path)
  (load-pgl ext-path)
  #?"# loaded extension from ${ext-path}")

(defun fmt-with (exp rest)
  "`exp' is the only expression and `rest' might include `:as' specifier."
  (ematch rest
    ((list* :as name body)
     #?"with ${(fmt exp)} as ${(fmt name)}:\n${(fmt-block body)}")
    (_ #?"with ${(fmt exp)}:\n${(fmt-block rest)}")))

(defun fmt (exp)
  (ematch exp
    ((list* 'defun name lambda-list body)
     (fmt-fn name lambda-list body))
    ((list* 'with exp rest)
     (fmt-with exp rest))
    ((list 'setf lhs rhs)
     (fmt-setf lhs rhs))
    ((cons 'pg-list args)
     (fmt-list args))
    ((cons 'pg-list-comp args)
     (apply #'fmt-list-comp args))
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
    ((list 'load-ext ext-path)
     (fmt-ext ext-path))
    ((guard x (atom x))
     (fmt-atom x))
    ((guard x (id-p x))
     (fmt-id x))
    ((guard x (lambda-p x))
     (apply #'fmt-lambda exp))
    ((cons fn args)
     (fmt-call fn args))))

(defmacro pgfmt (&rest body)
  (cl-strings:join (mapcar #'fmt body) :separator (make-string 2 :initial-element #\linefeed)))
