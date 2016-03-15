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
(defun start ()
  (game-loop))
