#|
  This file is a part of poker project.
  Copyright (c) 2015 Kono Yuya (takohati0821@gmail.com)
|#

#|
  Author: Kono Yuya (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage poker-asd
  (:use :cl :asdf))
(in-package :poker-asd)

(defsystem poker
  :version "0.1"
  :author "Kono Yuya"
  :license "MIT"
  :depends-on (:alexandria
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "poker" :depends-on ("util" "card" "hand" "game"))
                 (:file "game" :depends-on ("util" "card" "hand"))
                 (:file "hand" :depends-on ("util" "card"))
                 (:file "card" :depends-on ("util"))
                 (:file "util"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op poker-test))))
