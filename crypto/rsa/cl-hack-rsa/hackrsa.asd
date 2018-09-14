#|
  This file is a part of hackrsa project.
  Copyright (c) 2017 4hiziri (meirvg@gmail.com)
|#

#|
  tool for attacking rsa

  Author: 4hiziri (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage hackrsa-asd
  (:use :cl :asdf))
(in-package :hackrsa-asd)

(defsystem hackrsa
  :version "0.1"
  :author "4hiziri"
  :license ""
  :depends-on (:cl-annot :cl-prime)
  :components ((:module "src" :serial t
                :components
                ((:file "hackrsa")
		 (:file "common-modulus-attack")
		 (:file "low-public-exponent-attack")
		 (:file "fermat-rules-attack")
		 (:file "wiener-attack")
		 (:file "hastads-broadcast-attack"))))
  :description "tool for attacking rsa"
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
  :in-order-to ((test-op (test-op hackrsa-test))))
