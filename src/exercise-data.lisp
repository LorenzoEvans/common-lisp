(ql:quickload "cl-json")

(in-package #:cl)

(defpackage #:exercise-data
  (:use #:cl)
  (:export #:read-exercise-data
           #:exercise-name
           #:exercise-cases))

(in-package :exercise-data)

(defparameter *canonical-data-pathname-defaults*
  (make-pathname :directory '(:relative "../../problem-specifications/exercises")
                 :name "canonical-data"
                 :type "json"))

(defun canonical-data-pathname (exercise)
  (merge-pathnames (make-pathname :directory (list :relative exercise))
                   *canonical-data-pathname-defaults*))

(defun read-exercise-data (exercise)
  (let ((pathname (canonical-data-pathname exercise)))
    (when (probe-file pathname)
      (with-open-file (stream pathname)
        (cl-json:decode-json-strict stream)))))

(defun exercise-name (exercise-data)
  (cdr (assoc :exercise exercise-data)))

(defun exercise-cases (exercise-data)
  (cdr (assoc :cases exercise-data)))
