(in-package :cl-user)
(defpackage poker
  (:use :cl
        :cl-annot
        :alexandria
        :poker.card
        :poker.hand
        :poker.util
        :poker.game))

(in-package :poker)

(cl-annot:enable-annot-syntax)

(setf *random-state* (make-random-state t))

@export
(defun test (i)
  (labels ((f (a) (mapcar #'show-card a) (princ " ") (princ (poker-hand a)) (fresh-line)))
    (time (loop repeat i
                do (format t "~A~%"(poker-hand (take 5 (shuffle (copy-seq *all-cards*)))))))))

