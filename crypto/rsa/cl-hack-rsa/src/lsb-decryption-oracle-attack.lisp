(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defun lsb-decryption-oracle-attack (n m-list)
  "m-list is the list of decrypted result of c * (2^i)^e.
It implies this is the list of (2^i)m"
  (if (= (integer-length n) (length m-list))
      (let ((lsb-list (mapcar #'oddp m-list)))
	(labels ((binary-search (rest min max)
		   (cond
		     (rest min)
		     ((car rest) (binary-search (cdr rest) (truncate (- max min) 2) max))
		     (t (binary-search (cdr rest) min (truncate (- max min) 2))))))
	  (binary-search lsb-list 0 n)))))
