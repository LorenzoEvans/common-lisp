;; TODO
;; - input parameter cleanup
;; - assert-error
;; - production code stub
;; - example code file
;; - exercise comment in test file
;; - exercise version number in test file

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

(defun write-prologue (stream test-data)
  (let* ((name (cdr (assoc :exercise test-data)))
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
  (let ((cases (cdr (assoc :cases test-data)))
        (package (cdr (assoc :exercise test-data))))
    (dolist (case cases) (write-test stream package case))))

(defun make-exercise-directory (test-data)
  (ensure-directories-exist
   (exercise-directory-pathname (cdr (assoc :exercise test-data)))))

(defun make-test-code (test-data)
  (let* ((exercise (cdr (assoc :exercise test-data)))
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
  (let ((test-data (canonical-data exercise)))
    (if test-data
        (progn
          (make-exercise-directory test-data)
          (make-test-code test-data)
          (make-production-code test-data))
        (format t "No data found for exercise: ~A" exercise))))
