#|
  This file is a part of cl-prime project.
  Copyright (c) 2017 4hiziri (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prime-test-asd
  (:use :cl :asdf))
(in-package :cl-prime-test-asd)

(defsystem cl-prime-test
  :author "4hiziri"
  :license ""
  :depends-on (:cl-prime
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-prime"))))
  :description "Test system for cl-prime"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
