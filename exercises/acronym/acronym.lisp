; (in-package #:cl-user)
; (defpackage #:acronym
;   (:use #:common-lisp)
;   (:export #:acronym))
;
; (in-package #:acronym)

(defun acronym (lst)
 (cond ((eql (length '(lst)) nil) (nil))
       ((eql (length '(lst)) 1)
        (subseq (car lst) 0 1))
       ((> (length '(lst) 1)
           (dolist (x (length lst))
                  (concatenate 'string
                               (subseq (car lst) 0 1)))))))
