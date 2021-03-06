#|
  This file is a part of cl-prime project.
  Copyright (c) 2017 4hiziri (meirvg@gmail.com)
|#

#|
  Tool for prime numbers

  Author: 4hiziri (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-prime-asd
  (:use :cl :asdf))
(in-package :cl-prime-asd)

(defsystem cl-prime
  :version "0.1"
  :author "4hiziri"
  :license ""
  :depends-on (:cl-annot)
  :components ((:module "src"
                :components
                ((:file "cl-prime"))))
  :description "Tool for prime numbers"
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
  :in-order-to ((test-op (test-op cl-prime-test))))
