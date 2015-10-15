(in-package :cl-user)
(defpackage :poker.util
  (:use :cl)
  (:export :let1 :take :maximum :minimum :suffle))
(in-package :poker.util)

(defmacro let1 (var expr &body body)
  `(let ((,var ,expr))
     ,@body))

(defun take (n ls)
  (if (or (<= n 0) (null ls))
    nil
    (cons (car ls) (take (1- n) (cdr ls)))))

(defun maximum (lst) (reduce #'max lst))
(defun minimum (lst) (reduce #'min lst))

(setf *random-state* (make-random-state t))
(defun shuffle (seq)
  (alexandria:shuffle (copy-seq seq)))

