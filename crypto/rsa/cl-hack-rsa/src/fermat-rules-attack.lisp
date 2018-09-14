(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defun fermat-rules-attack (n)
  "Directly calculate prime factor, p * q = n."
  (flet ((init-a (n)
	   ;; big-integer raise overflow at sqrt
	   ;; but isqrt apply floor-function to answer
	   ;; need check for apply ceil-function
	   (let ((sqrt-n (isqrt n)))
	     (if (fast-perfect-square-p n)
		 sqrt-n
		 (1+ sqrt-n)))))
    (loop for a from (init-a n) to (/ (+ n 6) 9)
	  for square-b = (- (expt a 2) n) then (- (expt a 2) n)
	  when (fast-perfect-square-p square-b)
	    do (return (cons (+ a (isqrt square-b)) (- a (isqrt square-b)))))))
