(in-package :cl-user)
(defpackage hackrsa
  (:use :cl))
(in-package :hackrsa)

(cl-annot:enable-annot-syntax)

;; Util of math
@export
(defun extend-gcd (a b)
  "(gcd(a,b), x, y) | ax + by = gcd(a, b)

gcd(a, b) = gcd(b, a mod b)
ax + by = gcd(a, b), x and y exists
gcd(b, a mod b) = bx' + (a mod b)y'
then
bx' + (a mod b)y' = gcd(a, b) ... (1)
a mod b = a - (truncate a b) * b ... (2)
then (2) into (1)
bx' + ay' - (truncate a b) * b y' = gcd(a, b)
ay' + b(x' - (truncate a b)y') = gcd(a, b)
compare ax + by = gcd(a, b),
x = y', y = x' - (truncato a b)y'
"
  (if (= b 0)
      (list a 1 0)
      (let* ((egcd (extend-gcd b (mod a b)))
	     (g (first egcd))
	     (x (second egcd))
	     (y (third egcd)))
	(list g y (- x (* y (truncate a b)))))))

@export
(defun mod-expt (base exp modulus)
  "more effective expotential and modulus.
calculate mod at every step of exp."
  (assert (integerp exp))
  (labels ((inner-mod-expt (base exp modulus)
	     (if (< exp 0) ;; if exp < 0, cannot calc mod so simply return base^exp
		 (expt base exp)
		 (loop for acc = 1 then (if (evenp e) acc (mod (* acc b) modulus))
		       for b = (mod base modulus) then (if (evenp e) (mod (expt b 2) modulus) b)
		       for e = exp then (if (evenp e) (/ e 2) (1- e))
		       when (= e 0)
			 do (return acc)))))
    (if (>= exp 0)
	(inner-mod-expt base exp modulus)
	(/ 1 (inner-mod-expt base (- exp) modulus)))))

@export
(defun mod-inv (a m)
  (let ((egcd (extend-gcd a m)))
    (mod (second egcd) m)))

#+sbcl
(require :sb-mpfr)
@export
(defun n-root (x n &optional (prec 1024))
  "Return nth root of x"
  #-sbcl
  (progn
    (princ "n-root doesn't work, especialy when n is very large. Please use sbcl.")
    (round (expt x (/ 1d0 n)))) ;; Need mpfr bindings for other impls
  #+sbcl
  (sb-mpfr:coerce
   (sb-mpfr:with-precision prec
     (sb-mpfr:k-root (sb-mpfr:coerce x 'sb-mpfr:mpfr-float)
		     n))
   'integer))

@export
(defun gcd-attack (n1 n2)
  "Two n are shared prime is contained, it can calculate other prime."
  (gcd n1 n2))

(defun perfect-square-p (n)
  "if n's sqrt is integer, return t"
  (and (>= n 0)
       (= (expt (isqrt n) 2) n)))

(defparameter *table-sq-mod256* #(1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0
				  0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0
				  0 0 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0
				  1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0
				  0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 0
				  0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1
				  0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
				  0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0
				  0 1 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1 0
				  0 0 0 0 0 0 1 0 0 0 0 0 0))

(defun fast-perfect-square-p (n)
  "Filtering n by mod 256, 5, 7, 9, 13 and 17, this works more fast."
  (let ((sq-mod256 *table-sq-mod256*)
	(mt (list (cons 9 #(1 1 0 0 1 0 0 1 0))
		  (cons 5 #(1 1 0 0 1))
		  (cons 7 #(1 1 1 0 1 0 0))
		  (cons 13 #(1 1 0 1 1 0 0 0 0 1 1 0 1))
		  (cons 17 #(1 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1))))
	(a (mod n (* 5 7 9 13 17))))
    (if (/= (aref sq-mod256 (logand n #xff)) 0)
	(dolist (mt-val mt (perfect-square-p n))
	  (when (= (aref (cdr mt-val) (mod a (car mt-val))) 0)
	    (return nil)))
	nil)))

;; Util of RSA
@export
(defun decode-string (encoded-num)
  (labels ((inner-loop (num acc)
	     (if (> num 0)
		 (inner-loop (truncate num (expt 2 8)) (cons (mod num (expt 2 8)) acc))
		 acc)))
    (coerce (mapcar (lambda (x) (code-char x))
		    (inner-loop encoded-num nil))
	    'string)))

@export
(defun private-key (e p q)
  (mod-inv e (* (1- p) (1- q))))

@export
(defun decrypt (c d n)
  (mod-expt c d n))

@export
(defun encrypt (m n e)
  (mod-expt m e n))

;; Util
(defun zip (al bl) ;; TODO: extract util or search
  (labels ((inner-loop (al bl acc)
	     (if (or (null al)
		     (null bl))
		 (reverse acc)
		 (inner-loop (rest al)
			     (rest bl)
			     (cons (cons (first al)
					 (first bl))
				   acc)))))
    (inner-loop al bl nil)))
