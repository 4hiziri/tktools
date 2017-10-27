(in-package :cl-user)
(defpackage vigenere
  (:use :cl))
(in-package :vigenere)

(defun subseq-by-len (seq len)
  "(subseq-by-len '(1 2 3 4 5) 2) => '((1 2) (2 3) (3 4) (4 5))"
  (let ((ret nil))    
    (dotimes (n (- (length seq) (1- len)) (reverse ret))
      (push (subseq seq n (+ n len))
	    ret))))

(defun length-between-seq (seq1 seq2)
  (search ))
