(in-package :cl-user)
(defpackage :poker.card
  (:use :cl
        :poker.util)
  (:import-from :alexandria
                :iota))
(in-package :poker.card)

(cl-annot:enable-annot-syntax)

(defun rank-to-string (rank)
  (case rank
    (1  "A")
    (13 "K")
    (12 "Q")
    (11 "J")
    (t (write-to-string rank))))

(defun read-rank (str)
  (cond ((string-equal str "A") 1)
        ((string-equal str "K") 13)
        ((string-equal str "Q") 12)
        ((string-equal str "J") 11)
        (t (parse-integer str))))

(defun suit-to-string (suit)
  (case suit
    (0 "S")
    (1 "H")
    (2 "D")
    (3 "C")
    (t (write-to-string suit))))

(defun read-suit (str)
  (cond ((string-equal str "S") 0)
        ((string-equal str "H") 1)
        ((string-equal str "D") 2)
        ((string-equal str "C") 3)
        (t str)))

@export
(defun show-card (card)
  (princ (suit-to-string (car card)))
  (princ (rank-to-string (cdr card)))
  (princ "\\"))

@export
(defvar *all-cards* nil)

@export
(defun init-all-cards ()
  (setq *all-cards* nil)
  (dolist (suit (iota 4 :start 0))
    (dolist (rank (iota 13 :start 1))
      (push (cons suit rank) *all-cards*)))
  *all-cards*)

(init-all-cards)

(defun cardp (item)
  (member item *all-cards* :test #'equal))

