(in-package :cl-user)

(asdf:defsystem :x.let-star
  :description "value binder"
  :author "karol.skocik@gmail.com"
  :license "BSD compatible"
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "declarations")
               (:file "let-star")
               (:file "tests")))




