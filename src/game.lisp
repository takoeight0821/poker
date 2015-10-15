(in-package :cl-user)
(defpackage :poker.game
  (:use :alexandria
        :cl
        :cl-annot
        :poker.util
        :poker.card
        :poker.hand))

(in-package :poker.game)

(cl-annot:enable-annot-syntax)

(defparameter *game-score*  0)
(defparameter *player-hand* '())

@export
(defun game-loop ()
  (init-all-cards)
  (setq *all-cards* (shuffle  *all-cards*))
  (setq *player-hand* (deal-cards 5))
  (check-result)
  (princ "Continue? yes/no")
  (terpri)
  (when (string-equal "yes" (read-line))
    (game-loop))
  *game-score*)

(defun deal-cards (n)
  (let ((cards (take n *all-cards*)))
    (setq *all-cards* (nthcdr n *all-cards*))
    cards))

(defun check-result ()
  (format t "Result: ~A~%" (poker-hand *player-hand*))
  (incf *game-score* (hand-strength *player-hand*)))

