#|
  This file is a part of hackrsa project.
  Copyright (c) 2017 4hiziri (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage hackrsa-test-asd
  (:use :cl :asdf))
(in-package :hackrsa-test-asd)

(defsystem hackrsa-test
  :author "4hiziri"
  :license ""
  :depends-on (:hackrsa
	       :prove)
  :components ((:module "t"
                :components
                ((:file "hackrsa"))))
  :description "Test system for hackrsa"
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
