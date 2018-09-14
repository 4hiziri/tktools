(in-package :cl-user)
(defpackage cl-prime
  (:use :cl))
(in-package :cl-prime)

(cl-annot:enable-annot-syntax)

@export
(defun mod-expt (base exp modulus)
  "more effective expotential and modulus.
calculate mod at every step of exp."
  (if (< exp 0) ;; if exp < 0, cannot calc mod so simply return base^exp
      (expt base exp)
      (loop for acc = 1 then (if (evenp e) acc (mod (* acc b) modulus))
	    for b = (mod base modulus) then (if (evenp e) (mod (expt b 2) modulus) b)
	    for e = exp then (if (evenp e) (/ e 2) (1- e))
	    when (= e 0)
	      do (return acc))))

@export
(defun coprime (n)
  "return coprime number to n"
  (if (oddp n)
      2
      (loop for i from 3 by 2 do
	   (if (= (gcd n i) 1)
	       (return i)))))

@export
(defun fermat-test (p)
  "If a^(p-1) = 1 mod p, p is (almost) prime.
WARN: This test returns t against pseudo-prime and Carmichael number but these number isn't prime"
  (if (= (mod-expt (coprime p) (1- p) p) 1)
      t
      nil))

@export
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

