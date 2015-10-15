(in-package :cl-user)
(defpackage :poker.hand
  (:use :cl
        :cl-annot
        :poker.util
        :poker.card))
(in-package :poker.hand)

(cl-annot:enable-annot-syntax)

; ペアの総数を出す
(defun count-pair (cards)
  (if (= (length cards) 1)
    0
    (+
      ;carと同じ数字のカード枚数をカウント
      (count (cdar cards) (mapcar #'cdr (cdr cards)))
      (count-pair (cdr cards)))))

(defun max-rank (cards)
  (maximum (mapcar #'cdr cards)))

(defun min-rank (cards)
  (minimum (mapcar #'cdr cards)))

(defun sum-rank (cards)
  (reduce #'+ (mapcar #'cdr cards)))

(defun flushp (cards)
  (if (and (= (count (caar cards) (mapcar #'car (cdr cards))) 4)
           (< (count-pair cards) 4))
    'flush nil))

(defun straightp (cards)
  (if (and (= (count-pair cards) 0)
           (or (= (- (max-rank cards) (min-rank cards)) 4)
               (and (= (min-rank cards) 1)
                    (= (sum-rank cards) 47))))
    'straight nil))

(defun straight-flush-p (cards)
  (if (and (straightp cards)
           (flushp cards))
    'straight-flush nil))

(defun royal-flush-p (cards)
  (if (and (straight-flush-p cards)
           (= (sum-rank cards) 47))
    'royal-flush nil))

(defun four-of-a-kind-p (cards)
  (if (= (count-pair cards) 6)
    'four-of-a-kind nil))

(defun full-house-p (cards)
  (if (= (count-pair cards) 4)
    'full-house nil))

(defun three-of-a-kind-p (cards)
  (if (= (count-pair cards) 3)
    'three-of-a-kind nil))

(defun two-pair-p (cards)
  (if (= (count-pair cards) 2)
    'two-pair nil))

(defun one-pair-p (cards)
  (if (= (count-pair cards) 1)
    'one-pair nil))

@export
(defun poker-hand (cards)
  (or (royal-flush-p cards)
      (straight-flush-p cards)
      (four-of-a-kind-p cards)
      (full-house-p cards)
      (flushp cards)
      (straightp cards)
      (three-of-a-kind-p cards)
      (two-pair-p cards)
      (one-pair-p cards)
      'high-cards))

@export
(defun hand-strength (cards)
  (case (poker-hand cards)
    ((high-cards)      0)
    ((one-pair)        1)
    ((two-pair)        2)
    ((three-of-a-kind) 3)
    ((straight)        4)
    ((flush)           5)
    ((full-house)      6)
    ((four-of-a-kind)  7)
    ((straight-flush)  8)
    ((royal-flush)     9)))

(defun same-hand-strength (cards)
  (if (member 1 (mapcar #'cdr cards))
    14
    (car (sort (mapcar #'cdr cards) #'>))))

