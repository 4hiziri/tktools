#|
  This file is a part of subst project.
  Copyright (c) 2017 tkgsy
|#

#|
  Substitution cipher decrypto util

  Author: tkgsy
|#

(in-package :cl-user)
(defpackage subst-asd
  (:use :cl :asdf))
(in-package :subst-asd)

(defsystem subst
  :version "0.1"
  :author "tkgsy"
  :license ""
  :depends-on (:cl-annot)
  :components ((:module "src"
                :components
                ((:file "subst"))))
  :description "Substitution cipher decrypto util"
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
  :in-order-to ((test-op (test-op subst-test))))
