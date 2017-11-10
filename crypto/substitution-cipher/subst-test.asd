#|
  This file is a part of subst project.
  Copyright (c) 2017 tkgsy
|#

(in-package :cl-user)
(defpackage subst-test-asd
  (:use :cl :asdf))
(in-package :subst-test-asd)

(defsystem subst-test
  :author "tkgsy"
  :license ""
  :depends-on (:subst
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "subst"))))
  :description "Test system for subst"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
