(in-package :cl-user)

(defpackage :x.let-star
  (:use :common-lisp)
  (:shadow common-lisp:let*)
  (:export #:let*))

