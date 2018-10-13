(defsystem #:pigeon
  :description "s-exp based python"
  :author "Abhinav Tushar <lepisma@fastmail.com>"
  :license "GPLv3"
  :version "0.1.4"
  :depends-on (#:trivia
               #:cl-cut
               #:cl-interpol
               #:cl-strings)
  :components
  ((:file "package")
   (:module "src"
    :depends-on ("package")
    :serial t
    :components
    ((:file "vars")
     (:file "utils")
     (:file "read")
     (:file "predicates")
     (:file "transform")
     (:file "format")
     (:file "shell")
     (:file "pigeon")))))
