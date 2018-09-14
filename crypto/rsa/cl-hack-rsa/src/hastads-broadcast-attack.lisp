(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

;; Too late, need optimization

(defun chinese-remainder-theorem (n_list a_list)
  "a0 = x mod n0
   ...
   ai = x mod ni

   return x mod n0 * n1 * ... * ni"
  (labels ((congruence-one (n modular)
	     (let ((egcd (extend-gcd modular n)))
	       (* n (third egcd))))
	   (positivate (n adder)
	     (if (> n 0)
		 n
		 (positivate (+ n adder) adder))))
    (let ((reduced (reduce #'* n_list)))
      (positivate
       (reduce #'+
	       (mapcar (lambda (xa) (* (first xa)
				       (rest xa)))
		       (zip (mapcar (lambda (n) (congruence-one (/ reduced n) n))
				    n_list)
			    a_list)))
       reduced))))

@export
(defun hastads-broadcast-attack (n_list e c_list)
  (let ((me (chinese-remainder-theorem n_list c_list)))
    (round (n-root me e))))
