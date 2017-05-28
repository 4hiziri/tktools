;; more generalize
(defun binary-search (func trg bottom top &optional (error-range 100000))
  (let* ((mid (truncate (+ top bottom) 2))
	 (res (funcall func mid)))
    (cond ((<= (abs (- res trg)) error-range) mid)
	  ((< res trg) (binary-search func trg mid top error-range))
	  ((> res trg) (binary-search func trg bottom mid error-range))
	  (t (format t "error: ~A~%" mid)))))


