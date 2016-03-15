(in-package :cl-user)
(defpackage :poker.hand
  (:use :cl
        :cl-annot
        :poker.util
        :poker.card))
(in-package :poker.hand)

(cl-annot:enable-annot-syntax)

(defun count-pair (cards)
  "Count pairs."
  (if (= (length cards) 1)
    0
    (+
      ;carと同じ数字のカード枚数をカウント
     (count (rank (first cards)) (mapcar #'rank (rest cards)))
      (count-pair (rest cards)))))

(defun max-rank (cards)
  (maximum (mapcar #'rank cards)))

(defun min-rank (cards)
  (minimum (mapcar #'rank cards)))

(defun sum-rank (cards)
  (reduce #'+ (mapcar #'rank cards)))

(defun flushp (cards)
  (when (and (= (count (suit (first cards)) (mapcar #'suit (rest cards))) 4)
           (< (count-pair cards) 4))
    'flush))
(defun straightp (cards)
  (when (and (= (count-pair cards) 0)
             (or (= (- (max-rank cards) (min-rank cards)) 4)
                 (and (= (min-rank cards) 1)
                      (= (sum-rank cards) 47 ; (= (+ 1 13 12 11 10) 47) 
                         ))))
    'straight))

(defun straight-flush-p (cards)
  (when (and (straightp cards) (flushp cards))
    'straight-flush))

(defun royal-flush-p (cards)
  (when (and (straight-flush-p cards)
             (= (sum-rank cards) 47))
    'royal-flush))

(defun four-of-a-kind-p (cards)
  (when (= (count-pair cards) 6)
    'four-of-a-kind))

(defun full-house-p (cards)
  (when (= (count-pair cards) 4)
    'full-house))

(defun three-of-a-kind-p (cards)
  (when (= (count-pair cards) 3)
    'three-of-a-kind))

(defun two-pair-p (cards)
  (when (= (count-pair cards) 2)
    'two-pair))

(defun one-pair-p (cards)
  (when (= (count-pair cards) 1)
    'one-pair))

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

@export
(defun same-hand-strength (cards)
  (if (member 1 (mapcar #'rank cards))
    14                                  ; Aが最高
    (first (sort (mapcar #'rank cards) #'>))))
