(in-package :cl-user)
(defpackage :poker.util
  (:use :cl)
  (:export :take :maximum :minimum))
(in-package :poker.util)

(defun take (n ls)
  (if (or (<= n 0) (null ls))
    nil
    (cons (car ls) (take (1- n) (cdr ls)))))

(defun maximum (lst) (reduce #'max lst))
(defun minimum (lst) (reduce #'min lst))

(setf *random-state* (make-random-state t))

