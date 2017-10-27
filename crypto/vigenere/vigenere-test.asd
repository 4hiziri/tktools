#|
  This file is a part of vigenere project.
  Copyright (c) 2017 tkgsy
|#

(in-package :cl-user)
(defpackage vigenere-test-asd
  (:use :cl :asdf))
(in-package :vigenere-test-asd)

(defsystem vigenere-test
  :author "tkgsy"
  :license ""
  :depends-on (:vigenere
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "vigenere"))))
  :description "Test system for vigenere"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
