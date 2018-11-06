(ql:quickload "lisp-unit")
#-xlisp-test (load "book-store")

(defpackage #:book-store-test
  (:use #:common-lisp #:lisp-unit))

(in-package #:book-store-test)
#-xlisp-test
(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all)
