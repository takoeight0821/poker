(in-package :cl-user)
(defpackage poker-test
  (:use :cl
        :poker
        :poker.engine.hand
        :prove))
(in-package :poker-test)

;; NOTE: To run this test file, execute `(asdf:test-system :poker)' in your Lisp.

(plan nil)

(diag "Testing 'poker-hand'.")
;; TODO: add all pattern of poker-hand.
(is (poker-hand '((0 . 1) (0 . 10) (0 . 11) (0 . 12) (0 . 13))) 'royal-flush)
(is (poker-hand '((0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5))) 'straight-flush)

(finalize)
