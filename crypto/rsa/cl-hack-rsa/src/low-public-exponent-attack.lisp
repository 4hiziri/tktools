(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defun low-public-exponent-attack (c e &optional (limit 1000000))
  "If e is too small and m^e < n, this is success"
  (let ((estimate (round (n-root c e))))
    (loop for i from 0 to limit
	  if (= c (expt (- estimate i) e))
	    do (return (- estimate i))
	  else
	    if (= c (expt (+ estimate i) e))
	      do (return (+ estimate i)))))
