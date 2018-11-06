(ql:quickload "cl-json")

(defpackage :test-generator
  (:use :cl)
  (:export :generate))

(in-package :test-generator)

(defparameter *canonical-data-pathname-defaults*
  (make-pathname :directory '(:relative "../../problem-specifications/exercises")
                 :name "canonical-data"
                 :type "json"))

(defparameter *exercise-pathname-defaults*
  (make-pathname :directory '(:relative "../exercises")))

(defun exercise-pathname (exercise)
  (make-pathname :directory (list :relative exercise)))

(defun canonical-data-pathname (exercise)
  (merge-pathnames (exercise-pathname exercise)
                   *canonical-data-pathname-defaults*))

(defun exercise-directory-pathname (exercise)
  (merge-pathnames (exercise-pathname exercise) *exercise-pathname-defaults*))

(defun canonical-data (exercise)
  (let ((pathname (canonical-data-pathname exercise)))
    (when (probe-file pathname)
      (with-open-file (stream pathname)
        (cl-json:decode-json-strict stream)))))

(defun write-prologue (stream name)
  (format stream "~
(ql:quickload ~w)
#-xlisp-test (load ~w)

(defpackage #:~a
  (:use #:common-lisp #:lisp-unit))

(in-package #:~:*~a)
" "lisp-unit" name (format nil "~a-test" name)))

(defun write-epilogue (stream)
  (format stream "~
#-xlisp-test
(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all)
"))

(defun write-tests (stream tests-data)
  (format stream "~&~&"))

(defun make-tests (test-data)
  (let* ((exercise (cdr (assoc :exercise test-data)))
         (exercise-directory (exercise-directory-pathname exercise))
         (test-file (make-pathname :name (format nil "~A-test" exercise)
                                   :type "lisp"
                                   :defaults exercise-directory)))
    (ensure-directories-exist exercise-directory :verbose t)
    (with-open-file (stream test-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (write-prologue stream exercise)
      (write-tests stream test-data)
      (write-epilogue stream))))

(defun generate (exercise)
  (let ((test-data (canonical-data exercise)))
    (if test-data
        (make-tests test-data)
        (format t "No data foun for exercise: ~A" exercise))))
