;; TODO
;; - input parameter cleanup
;; - assert-error
;; - production code stub
;; - example code file
;; - exercise comment in test file
;; - exercise version number in test file

(ql:quickload "cl-json")

(load "../src/exercise-data")

(defpackage :test-generator
  (:use :cl :exercise-data)
  (:export :generate))

(in-package :test-generator)

(defparameter *exercise-pathname-defaults*
  (make-pathname :directory '(:relative "../exercises")))

(defun exercise-directory-pathname (exercise)
  (merge-pathnames (make-pathname :directory (list :relative exercise))
                   *exercise-pathname-defaults*))

(defun write-prologue (stream test-data)
  (let* ((name (exercise-name test-data))
         (test-package (format nil "~a-test" name)))
    (format stream "~
(ql:quickload ~w)
#-xlisp-test (load ~w)

(defpackage #:~a
  (:use #:common-lisp #:lisp-unit))

(in-package #:~a)" "lisp-unit" name test-package test-package))
  (terpri stream))


(defun write-epilogue (stream test-data)
  (declare (ignore test-data))
  (format stream "#-xlisp-test
(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all))")
  (terpri stream))

(defun write-test (stream package case)
  (let ((description (cdr (assoc :description case)))
        (property (cdr (assoc :property case)))
        (input (cdr (assoc :input case)))
        (expected (cdr (assoc :expected case))))

    (format stream "(define-test ~A
  (assert-equal
      ~S
      (~A:~A ~S)))" (substitute #\- #\space description)
      expected package property input))

  (terpri stream))


(defun write-tests (stream test-data)
  (let ((cases (exercise-cases test-data))
        (package (exercise-name test-data)))
    (dolist (case cases) (write-test stream package case))))

(defun make-exercise-directory (test-data)
  (ensure-directories-exist
   (exercise-directory-pathname (exercise-name test-data))))

(defun make-test-code (test-data)
  (let* ((exercise (exercise-name test-data))
         (exercise-directory (exercise-directory-pathname exercise))
         (test-file (make-pathname :name (format nil "~A-test" exercise)
                                   :type "lisp"
                                   :defaults exercise-directory)))
    (with-open-file (stream test-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (write-prologue stream test-data)
      (write-tests stream test-data)
      (write-epilogue stream test-data))))

(defun make-production-code (test-data)
  (declare (ignore test-data)))

(defun generate (exercise)
  (let ((test-data (read-exercise-data exercise)))
    (if test-data
        (progn
          (make-exercise-directory test-data)
          (make-test-code test-data)
          (make-production-code test-data))
        (format t "No data found for exercise: ~A" exercise))))
