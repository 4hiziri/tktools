#|
  This file is a part of vigenere project.
  Copyright (c) 2017 tkgsy
|#

#|
  Toolset for decrypting vigenere

  Author: tkgsy
|#

(in-package :cl-user)
(defpackage vigenere-asd
  (:use :cl :asdf))
(in-package :vigenere-asd)

(defsystem vigenere
  :version "0.1"
  :author "tkgsy"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "vigenere"))))
  :description "Toolset for decrypting vigenere"
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
  :in-order-to ((test-op (test-op vigenere-test))))
