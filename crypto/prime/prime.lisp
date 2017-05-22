

(defun mod-expt (base exp m)
     (cond ((= exp 0) 1)
	   ((evenp exp) (mod (expt (mod-expt base (/ exp 2) m) 2) m))
	   (t (mod (* base (mod-expt base (- exp 1) m)) m))))

(defun coprime (n)
  (if (oddp n)
      2
      (loop for i from 3 by 2 do
	   (if (= (gcd n i) 1)
	       (return i)))))

(defun fermat-test (p)
  (if (/= (mod-expt (coprime p) (1- p) p) 1)
      nil
      t))


;; test
(defun prime-p (p)
  (if (= p 1)
      nil
      (flet ((next (n)
	       (if (= n 2)
		   3
		   (+ n 2))))
	(do ((i 2 (next i)))
	    ((> i (floor (sqrt p))) p)
	  (if (= (mod p i) 0)
	      (return nil))))))
