#|
  This file is a part of poker project.
  Copyright (c) 2015 Kono Yuya (takohati0821@gmail.com)
|#

(in-package :cl-user)
(defpackage poker-test-asd
  (:use :cl :asdf))
(in-package :poker-test-asd)

(defsystem poker-test
  :author "Kono Yuya"
  :license "MIT"
  :depends-on (:poker
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "poker"))))
  :description "Test system for poker"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
