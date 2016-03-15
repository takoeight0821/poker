(ql:quickload :alexandria)

(in-package :cl-user)
(defpackage :poker
  (:use :cl)
  (:import-from :alexandria
   :iota
   :shuffle))
(in-package :poker)

(defun take (n ls)
  (if (or (<= n 0) (null ls))
      nil
      (cons (car ls) (take (1- n) (cdr ls)))))

(defun maximum (ls) (reduce #'max ls))
(defun minimum (ls) (reduce #'min ls))

(defun card (suit rank)
  (cons suit rank))
(defun suit (card)
  (car card))
(defun rank (card)
  (cdr card))

(defun rank-to-string (rank)
  (case rank
    (1 "A")
    (13 "K")
    (12 "Q")
    (11 "J")
    (t (write-to-string rank))))

(defun string-to-rank (str)
  (cond ((string-equal str "A") 1)
        ((string-equal str "K") 13)
        ((string-equal str "Q") 12)
        ((string-equal str "J") 11)
        (t (parse-integer str))))

(defun suit-to-string (suit)
  (case suit
    (0 "S") (1 "H") (2 "D") (3 "C") (t (write-to-string suit))))

(defun string-to-suit (str)
  (cond ((string-equal str "S") 0)
        ((string-equal str "H") 1)
        ((string-equal str "D") 2)
        ((string-equal str "C") 3)
        (t str)))

(defun show-card (card)
  (princ (suit-to-string (suit card)))
  (princ (rank-to-string (rank card)))
  (princ "\\"))

(defvar *all-cards* nil)
(defun init-all-cards ()
  (setq *all-cards* nil)
  (dolist (suit (iota 4 :start 0))
    (dolist (rank (iota 13 :start 1))
      (push (card suit rank) *all-cards*)))
  *all-cards*)
(init-all-cards)

(defun cardp (item)
  (member item *all-cards* :test #'equal))

(defun count-pair (cards)
  "ペアの総数を数える"
  (if (= (length cards) 1)
      0
      (+ ;carと同じ数字のカード枚数をカウント
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
  (if (member 1 (mapcar #'rank cards))
    14                                  ; Aが最高
    (first (sort (mapcar #'rank cards) #'>))))

;;; TODO: ゲームエンジンとreplの実装
