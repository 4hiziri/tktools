(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

@export
(defun common-modulus-attack (c1 c2 e1 e2 n)
  "If the same plain-texts are encrypted another e, this can get private-key D."
  (flet ((inner-solve (c1 c2 s1 s2)
	   (mod (* (mod-expt c1 s1 n)
		   (mod-expt c2 s2 n))
		n)))
    (let* ((s (extend-gcd e1 e2))
	   (s1 (second s))
	   (s2 (third s)))
      (cond ((< s1 0) (inner-solve (mod-inv c1 n) c2 (- s1) s2))
	    ((< s2 0) (inner-solve c1 (mod-inv c2 n) s1 (- s2)))
	    (t (inner-solve c1 c2 s1 s2))))))

