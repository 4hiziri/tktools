(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

(defun chinese-remainder-theorem (n_list a_list)
  "a0 = x mod n0
   ...
   ai = x mod ni
   return x mod n0 * n1 * ... * ni"
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (labels ((congruence-one (n modular)
	     (let ((egcd (extend-gcd modular n)))
	       (* n (third egcd)))))
    (let ((reduced (reduce #'* n_list)))
      (mod (reduce (lambda (acc xa)
		     (+ acc
			(* (congruence-one (/ reduced (first xa)) (first xa))
			   (rest xa))))
		   (zip n_list a_list)
		   :initial-value 0)
	   reduced))))

@export
(defun hastads-broadcast-attack (n_list e c_list)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((me (chinese-remainder-theorem n_list c_list)))
    (round (n-root me e))))
